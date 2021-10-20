package guardian

import cats.data.EitherT
import cats.implicits._
import cats.{Applicative, Monad}
import com.hsoft.scenario.status.ScenarioStatus
import com.ingalys.imc.BuySell
import com.ingalys.imc.order.Order
import guardian.Algo._
import guardian.Entities.OrderAction.{CancelOrder, InsertOrder, UpdateOrder}
import guardian.Entities.{CustomId, OrderAction, Portfolio, PutCall, RepoOrder}
import guardian.Error.UnknownError
import horizontrader.services.collectors.persistent.ActiveOrderDescriptorView

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import scala.math.BigDecimal.RoundingMode

class Algo[F[_]: Applicative: Monad](
    val liveOrdersRepo: LiveOrdersRepoAlgebra[F],
    val portfolioRepo: UnderlyingPortfolioAlgebra[F],
    val pendingOrdersRepo: PendingOrdersAlgebra[F],
    val pendingCalculationRepo: PendingCalculationAlgebra[F],
    underlyingSymbol: String,
    lotSize: Int,
    sendOrder: OrderAction => Order,
    logAlert: String => Unit,
    logInfo: String => Unit,
    logError: String => Unit
) {
  def createOrderActions(order: Order): F[List[OrderAction]] =
    for {
      a <- liveOrdersRepo.getOrdersByTimeSortedDown(underlyingSymbol)
      _ = logInfo(s"Algo 1. Live orders: $a")
      liveQty <- Monad[F].pure(calcTotalQty(a.map(_.order)))
      c       <- portfolioRepo.get(underlyingSymbol)
      d <- Monad[F].pure {
        val totalResidual = order.getQuantityL
        val position      = if (order.getBuySell == BuySell.BUY) Long.MaxValue else roundQtyByLotSize(c.position)
        logInfo(
          s"""Algo 2a. Total residual: $totalResidual, live orders: $liveQty, portfolio: ${if (
            position == Long.MaxValue
          ) "INFINITY"
          else position.toString}"""
        )
        if (a.isEmpty) {
          logInfo(
            s"Algo 2b. Live order is empty. Will create a new insert with qty: ${order.getQuantityL}"
          )
          List(createInsertOrder(totalResidual, position, liveQty, order.getPrice, order.getBuySell))
        } else {
          if (order.getBuySell != a.head.order.getBuySell) {
            logInfo(
              s"Algo 2b. Direction changed from ${a.head.order.getBuySell} to ${order.getBuySell}. Cancel all live orders and create a new insert with qty: ${order.getQuantityL}"
            )
            a.map(createCancelOrder) :+ createInsertOrder(
              totalResidual,
              position,
              0L, // because live orders are cancelled
              order.getPrice,
              order.getBuySell
            ) // cancels are first
          } else {
            if (totalResidual < liveQty) {
              logInfo(
                s"Algo 2b. Calculated qty is smaller than live orders. Will update or cancel."
              )
              trimLiveOrders(a, order.getQuantityL, ListBuffer.empty)
            } else if (totalResidual == liveQty) {
              logInfo(
                s"Algo 2b. Calculated qty is the same as live orders. Will do nothing."
              )
              List.empty[OrderAction]
            } else {
              logInfo(
                s"Algo 2b. Calculated qty is larger than live orders. Will create a new order."
              )
              List(createInsertOrder(totalResidual, position, liveQty, order.getPrice, order.getBuySell))
            }
          }
        }
      }
    } yield d

  def roundQtyByLotSize(qty: Long): Long =
    BigDecimal(qty).setScale(Math.log10(lotSize).toInt * -1, RoundingMode.HALF_EVEN).toLong

  def roundOrderActionByLotSize(action: OrderAction): OrderAction = {
    action match {
      case InsertOrder(order) =>
        InsertOrder(cloneOrderWithNewQty(order, roundQtyByLotSize(order.getQuantityL)))
      case UpdateOrder(a, order) =>
        UpdateOrder(a, cloneOrderWithNewQty(order, roundQtyByLotSize(order.getQuantityL)))
      case a @ CancelOrder(_, _) => a
    }
  }

  def removeZeroQty(action: OrderAction): Boolean =
    action match {
      case InsertOrder(order)    => order.getQuantityL != 0L
      case UpdateOrder(_, order) => order.getQuantityL != 0L
      case CancelOrder(_, _)     => true
    }

  def calcTotalQty(orders: List[Order]): Long = orders.foldLeft(0L)((a: Long, b: Order) => a + b.getQuantityL)

  val createCancelOrder: RepoOrder => OrderAction = o => CancelOrder(o.orderView, o.order)

  def createInsertOrder(
      totalResidual: Long,
      position: Long,
      liveQty: Long,
      price: Double,
      buySell: Int
  ): OrderAction = {
    val o = new Order()
    o.setPrice(price)
    o.setBuySell(buySell)
    o.setCustomField(CustomId.field, CustomId.generate.v)
    val qty = if (totalResidual >= position) position - liveQty else totalResidual - liveQty
    o.setQuantity(qty)
    InsertOrder(o)
  }

  @tailrec
  final def trimLiveOrders(
      liveOrders: List[RepoOrder],
      calculatedQuantity: Long,
      res: ListBuffer[OrderAction]
  ): List[OrderAction] = {
    val head :: rest = liveOrders
    val liveQty      = rest.foldLeft(0L)((x: Long, y: RepoOrder) => x + y.order.getQuantityL)
    if (liveQty < calculatedQuantity) {
      val remainder = calculatedQuantity - liveQty
      val o         = cloneModifyOrder(head.order, remainder, head.order.getPrice, head.order.getBuySell)
      res += UpdateOrder(head.orderView, o)
      res.toList
    } else if (liveQty == calculatedQuantity) {
      res += CancelOrder(head.orderView, head.order)
      res.toList
    } else {
      res += CancelOrder(head.orderView, head.order)
      trimLiveOrders(rest, calculatedQuantity, res)
    }
  }

  def cloneModifyOrder(o: Order, newQty: Long, newPrice: Double, newDirection: Int): Order = {
    val newOrder = o.deepClone().asInstanceOf[Order]
    newOrder.setPrice(newPrice)
    newOrder.setQuantity(newQty)
    newOrder.setBuySell(newDirection)
    newOrder
  }

  def updateLiveOrders(activeOrderDescriptorView: ActiveOrderDescriptorView, executedOrder: Order): F[Unit] =
    activeOrderDescriptorView.getAction match {
      case 3 => liveOrdersRepo.removeOrder(underlyingSymbol, executedOrder.getId)
      case _ =>
        if (activeOrderDescriptorView.getMarketStatus == "Terminated") {
          liveOrdersRepo.removeOrder(underlyingSymbol, executedOrder.getId)
        } else {
          liveOrdersRepo.putOrder(underlyingSymbol, RepoOrder(activeOrderDescriptorView, executedOrder))
        }
    }

  def process(preProcess: EitherT[F, Error, Order]): F[List[OrderAction]] =
    (for {
      a <- preProcess
      b <- EitherT.rightT[F, Error](cloneModifyOrder(a, roundQtyByLotSize(a.getQuantityL), a.getPrice, a.getBuySell))
      _ = logInfo(s"Algo 0. Start algo. Order with qty rounded: ${b.getQuantityL}")

      c <- EitherT.right[Error](createOrderActions(b))
      _ = logInfo(s"Algo 4. Calculation result: $c")
      d <- EitherT.right[Error](c.map(roundOrderActionByLotSize(_).pure[F]).sequence)
      _ = logInfo(s"Algo 5. Qty after rounding: $d")
      e <- EitherT.rightT[F, Error](d.filter(removeZeroQty))
      _ = logInfo(s"Algo 6. Removed zero qty: $e")
    } yield e).value.map {
      case Right(v) => v
      case Left(e) =>
        logAlert(s"Algo 6e: ${e.msg}")
        List.empty[OrderAction]
    }

  def getPendingOrderAction(customId: CustomId): EitherT[F, Error, OrderAction] =
    for {
      a <- EitherT.liftF[F, Error, Option[OrderAction]](pendingOrdersRepo.get(customId))
      b <- EitherT.fromEither[F](
        a.fold[Either[Error, OrderAction]](Left(UnknownError(s"Pending order not found, custom id:${customId.v}")))(p =>
          Right(p)
        )
      )
    } yield b

  def checkAndUpdatePendingOrderAndCalculation(): EitherT[F, Error, Unit] =
    for {
      a <- EitherT.right[Error](pendingOrdersRepo.isEmpty)
      _ <- EitherT.right[Error](if (a) pendingCalculationRepo.activate() else Applicative[F].pure(()))
      b <- EitherT.fromEither[F](isPendingError(a))
    } yield b

  private def isPendingError(a: Boolean): Either[Error, Unit] = Either.cond(a, (), Error.PendingError)
  // CALLED WHEN SIGNAL

  def handleOnPortfolio(qty: Long): F[Unit] =
    for {
      _ <- portfolioRepo.put(underlyingSymbol, Portfolio(underlyingSymbol, qty))
    } yield ()

  // called everytime ul price changes
  def handleOnSignal(preProcess: EitherT[F, Error, Order]): F[Either[Error, Unit]] =
    (for {
      _ <- checkAndUpdatePendingOrderAndCalculation()
      a <- EitherT.right[Error](process(preProcess))
      _ <- EitherT.right[Error](a.map(p => sendOrder(p).pure[F]).sequence)
      _ <- EitherT.right[Error](a.map(pendingOrdersRepo.put).sequence)
    } yield ()).value.map {
      case Right(_) => Right(())
      case Left(e) =>
        logAlert(e.msg)
        Left(e)
    }

  def convertToOrder(a: ActiveOrderDescriptorView, sentOrder: Order, underlyingOrderCustomId: CustomId): Order = {
    val newOrder = new Order()
    newOrder.setId(sentOrder.getId)
    newOrder.setPrice(sentOrder.getPrice)
    newOrder.setQuantity(a.getRQtyL) // remaining quantity
    newOrder.setBuySell(sentOrder.getBuySell)
    newOrder.setCustomField(CustomId.field, underlyingOrderCustomId.v)
    newOrder
  }

  def handleOnOrderAck(
      activeOrderDescriptorView: ActiveOrderDescriptorView,
      preProcess: EitherT[F, Error, Order]
  ): EitherT[F, Error, Unit] =
    for {
      sentOrder <- EitherT.rightT[F, Error](activeOrderDescriptorView.getOrderCopy)
      _ = logAlert(s"Execution Status ${activeOrderDescriptorView.getExecutionStatus}")
      _ = logAlert(s"Market Status ${activeOrderDescriptorView.getMarketStatus}")
      _ = logAlert(s"Action ${activeOrderDescriptorView.getAction}")

      customId = CustomId.fromOrder(sentOrder)
      executedOrder <- EitherT.rightT[F, Error](convertToOrder(activeOrderDescriptorView, sentOrder, customId))
      _ = logInfo(s"Agent 100. Start handling ack Id: ${sentOrder.getId} CustomId: $customId")
      _ <- EitherT.right[Error](updateLiveOrders(activeOrderDescriptorView, executedOrder))
      _ <- EitherT.right[Error](pendingOrdersRepo.remove(customId))
      d <- EitherT.right[Error](pendingCalculationRepo.shouldRecalculate)
      _ <- EitherT.right[Error](pendingCalculationRepo.deactivate())
      _ <- if (d) EitherT(handleOnSignal(preProcess)) else EitherT.rightT[F, Error](())
    } yield ()

  def handleOnOrderNak(
      customId: CustomId,
      errorMsg: String
  ): EitherT[F, Error, Unit] =
    for {
      _ <- EitherT.rightT[F, Error](logAlert(errorMsg))
      _ <- EitherT.right[Error](pendingOrdersRepo.remove(customId))
      _ <- EitherT.right[Error](pendingCalculationRepo.deactivate())
    } yield ()

  def handleOnUlProjectedPrice(): EitherT[F, Error, Unit] =
    for {
      a <- EitherT.right[Error](liveOrdersRepo.getOrdersByTimeSortedDown(underlyingSymbol))
      b <- EitherT.rightT[F, Error](a.map(p => CancelOrder(p.orderView, p.order)))
      _ <- EitherT.right[Error](b.map(p => sendOrder(p).pure[F]).sequence)
      _ <- EitherT.right[Error](b.map(pendingOrdersRepo.put).sequence)
      _ <- EitherT.rightT[F, Error](pendingCalculationRepo.activate())
    } yield ()
}

object Algo {
  def apply[F[_]: Applicative: Monad](
      underlyingSymbol: String,
      lotSize: Int,
      sendOrder: OrderAction => Order,
      logAlert: String => Unit,
      logInfo: String => Unit,
      logError: String => Unit
  ): Algo[F] =
    new Algo(
      liveOrdersRepo = new LiveOrdersInMemInterpreter[F],
      portfolioRepo = new UnderlyingPortfolioInterpreter[F],
      pendingOrdersRepo = new PendingOrdersInMemInterpreter[F],
      pendingCalculationRepo = new PendingCalculationInMemInterpreter[F],
      underlyingSymbol = underlyingSymbol,
      lotSize = lotSize,
      sendOrder = sendOrder,
      logAlert = logAlert,
      logInfo = logInfo,
      logError = logError
    )

  def cloneOrderWithNewQty(order: Order, newQty: Long): Order = {
    val o = new Order()
    o.setId(order.getId)
    o.setQuantity(newQty)
    o.setPrice(order.getPrice)
    o.setBuySell(order.getBuySell)
    o.setCustomField(CustomId.field, order.getCustomField(CustomId.field).asInstanceOf[String])
    o
  }

  def createPreProcessOrder(qty: Long, price: Double, buySell: Int, customId: CustomId): Order = {
    val o = new Order()
    o.setQuantity(qty)
    o.setPrice(price)
    o.setBuySell(buySell)
    o.setCustomField(CustomId.field, customId)
    o
  }

  @tailrec
  def getPriceAfterTicks(isPlus: Boolean, price: BigDecimal, ticks: Int = 5): BigDecimal = {
    if (ticks < 1) {
      price
    } else {
      getPriceAfterTicks(
        isPlus,
        (if (isPlus) price + SETChkSpread(price, isPlus) else price - SETChkSpread(price, isPlus))
          .setScale(2, BigDecimal.RoundingMode.HALF_UP),
        ticks - 1
      )
    }
  }

  def SETChkSpread(refPrice: BigDecimal, isUp: Boolean): Double = {
    val c: List[(BigDecimal, Double, Double)] =
      List(
        (BigDecimal("0"), 0.0, 0.01),
        (BigDecimal("2"), 0.01, 0.02),
        (BigDecimal("5"), 0.02, 0.05),
        (BigDecimal("10"), 0.05, 0.1),
        (BigDecimal("25"), 0.1, 0.25),
        (BigDecimal("100"), 0.25, 0.5),
        (BigDecimal("200"), 0.5, 1),
        (BigDecimal("400"), 1, 2)
      )
    val x = c.filter(p => refPrice >= p._1).last
    if (refPrice == x._1 && !isUp) x._2 else x._3
  }

  case class MyScenarioStatus(priceOnMarket: Double, qtyOnMarketL: Long)

  case class DW(
      uniqueId: String,
      projectedPrice: Option[Double] = None,
      projectedVol: Option[Long] = None,
      delta: Option[Double] = None,
      putCall: Option[PutCall] = None,
      marketSells: Seq[MyScenarioStatus] = Seq.empty,
      marketBuys: Seq[MyScenarioStatus] = Seq.empty,
      ownSellStatusesDefault: Seq[MyScenarioStatus] = Seq.empty,
      ownBuyStatusesDefault: Seq[MyScenarioStatus] = Seq.empty,
      ownSellStatusesDynamic: Seq[MyScenarioStatus] = Seq.empty,
      ownBuyStatusesDynamic: Seq[MyScenarioStatus] = Seq.empty
  )

  def predictResidual(
      marketBuys: Seq[MyScenarioStatus],
      marketSells: Seq[MyScenarioStatus],
      ownBuyStatusesDefault: Seq[MyScenarioStatus],
      ownSellStatusesDefault: Seq[MyScenarioStatus],
      ownBuyStatusesDynamic: Seq[MyScenarioStatus],
      ownSellStatusesDynamic: Seq[MyScenarioStatus],
      dwMarketProjectedPrice: Double,
      dwMarketProjectedQty: Long,
      signedDelta: Double
  ): Long = {
    val bdOwnBestBidDefault = BigDecimal(
      ownBuyStatusesDefault.sortWith(_.priceOnMarket < _.priceOnMarket).lastOption.map(_.priceOnMarket).getOrElse(0.0)
    ).setScale(2, RoundingMode.HALF_EVEN)
    val bdOwnBestAskDefault = BigDecimal(
      ownSellStatusesDefault
        .sortWith(_.priceOnMarket < _.priceOnMarket)
        .headOption
        .map(_.priceOnMarket)
        .getOrElse(Int.MaxValue.toDouble)
    ).setScale(2, RoundingMode.HALF_EVEN)
    val bdOwnBestBidDynamic = BigDecimal(
      ownBuyStatusesDynamic.sortWith(_.priceOnMarket < _.priceOnMarket).lastOption.map(_.priceOnMarket).getOrElse(0.0)
    ).setScale(2, RoundingMode.HALF_EVEN)
    val bdOwnBestAskDynamic = BigDecimal(
      ownSellStatusesDynamic
        .sortWith(_.priceOnMarket < _.priceOnMarket)
        .headOption
        .map(_.priceOnMarket)
        .getOrElse(Int.MaxValue.toDouble)
    ).setScale(2, RoundingMode.HALF_EVEN)
    val qty: Long = {
      val bdOwnBestBid =
        if (bdOwnBestBidDefault <= bdOwnBestBidDynamic) bdOwnBestBidDynamic else bdOwnBestBidDefault
      val bdOwnBestAsk =
        if (bdOwnBestAskDefault <= bdOwnBestAskDynamic) bdOwnBestAskDefault else bdOwnBestAskDynamic
      val sumMktVolBid = marketBuys
        .filter(p => {
          p.priceOnMarket > bdOwnBestBid || p.priceOnMarket == 0.0
        })
        .map(_.qtyOnMarketL)
        .sum
      val sumMktVolAsk = marketSells
        .filter(p => {
          p.priceOnMarket < bdOwnBestAsk || p.priceOnMarket == 0.0
        })
        .map(_.qtyOnMarketL)
        .sum
      /*
       if (bdOwnBestBid >= dwMarketProjectedPrice) { //Mathed Buy
  dwMarketProjectedQty - sumMktVolBid
 } else if (bdOwnBestAsk <= dwMarketProjectedPrice) { //Matched Sell
            dwMarketProjectedQty - sumMktVolAsk
 } else {
          0
       }
       */
      if (bdOwnBestBid >= dwMarketProjectedPrice) { //Matched Buy
        (dwMarketProjectedQty - sumMktVolBid) * -1
      } else if (bdOwnBestAsk <= dwMarketProjectedPrice) { //Matched Sell
        dwMarketProjectedQty - sumMktVolAsk
      } else {
        0
      }
    }
    // CALL dw buy, order is positive , delta is positive, buy dw-> sell ul
    // PUT dw, buy, order is positive, delta is negative, buy dw -> buy ul
    // CALL dw sell, order is negative, delta is positive, sell dw -> buy ul
    // PUT dw sell, order is negative, delta is negative, sell dw -> sell ul
    BigDecimal(qty * signedDelta)
      .setScale(0, RoundingMode.HALF_EVEN)
      .toLong // positive = buy ul, negative = sell ul
  }
}
