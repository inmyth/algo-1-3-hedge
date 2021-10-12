package guardian

import cats.data.EitherT
import cats.implicits._
import cats.{Applicative, Id, Monad}
import com.ingalys.imc.BuySell
import com.ingalys.imc.dict.Element
import com.ingalys.imc.order.Order
import guardian.Algo._
import guardian.Entities.OrderAction.{CancelOrder, InsertOrder, UpdateOrder}
import guardian.Entities.PutCall.{CALL, PUT}
import guardian.Entities.{CustomId, Direction, OrderAction, Portfolio, PutCall, RepoOrder}
import guardian.Error.UnknownError
import horizontrader.plugins.hmm.connections.service.IDictionaryProvider
import horizontrader.services.collectors.persistent.ActiveOrderDescriptorView
import horizontrader.services.instruments.InstrumentDescriptor

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

class Algo[F[_]: Applicative: Monad](
    val liveOrdersRepo: LiveOrdersRepoAlgebra[F],
    val portfolioRepo: UnderlyingPortfolioAlgebra[F],
    val pendingOrdersRepo: PendingOrdersAlgebra[F],
    val pendingCalculationRepo: PendingCalculationAlgebra[F],
    underlyingSymbol: String,
    lotSize: Int,
    sendOrder: (OrderAction) => Order,
    logAlert: String => Unit,
    logInfo: String => Unit,
    logError: String => Unit
) {
  def createOrderActions(order: Order): F[List[OrderAction]] =
    for {
      a <- liveOrdersRepo.getOrdersByTimeSortedDown(underlyingSymbol)
      _ = logInfo(s"Algo 1. Live orders: $a")
      b <- Monad[F].pure(calcTotalQty(a.map(_.order)))
      _ = logInfo(s"Algo 2. Live orders total qty: $b")
      c <- portfolioRepo.get(underlyingSymbol)
      d <- Monad[F].pure {
        val desiredQty   = order.getQuantityL - b
        val availableQty = if (order.getBuySell == BuySell.BUY) Long.MaxValue else c.position - b
        logInfo(
          s"Algo 3. Desired qty $desiredQty = Total residual qty ${order.getQuantityL} - Live orders total qty $b, Portfolio = ${c.position}"
        )
        if (a.isEmpty) {
          createInsertOrder(order.getQuantityL, order.getPrice, order.getBuySell, availableQty).toList
        } else {
          if (order.getBuySell != a.head.order.getBuySell) {
            a.map(createCancelOrder) ++ createInsertOrder(
              order.getQuantityL,
              order.getPrice,
              order.getBuySell,
              c.position
            ).toList // cancels are first
          } else {
            if (desiredQty < 0L) {
              trimLiveOrders(a, order.getQuantityL, ListBuffer.empty)
            } else {
              createInsertOrder(desiredQty, order.getPrice, order.getBuySell, availableQty).toList
            }
          }
        }
      }
    } yield d

  def roundDownQty(action: OrderAction, lotSize: Int): OrderAction =
    action match {
      case InsertOrder(order) =>
        val rounded = order.getQuantityL / lotSize * lotSize
        InsertOrder(cloneOrderWithNewQty(order, rounded))
      case UpdateOrder(a, order) =>
        val rounded = order.getQuantityL / lotSize * lotSize
        UpdateOrder(a, cloneOrderWithNewQty(order, rounded))
      case a @ CancelOrder(_, _) => a
    }

  def removeZeroQty(action: OrderAction): Boolean =
    action match {
      case InsertOrder(order)    => order.getQuantityL != 0L
      case UpdateOrder(_, order) => order.getQuantityL != 0L
      case CancelOrder(_, order) => order.getQuantityL != 0L
    }

  def calcTotalQty(orders: List[Order]): Long = orders.foldLeft(0L)((a: Long, b: Order) => a + b.getQuantityL)

  val createCancelOrder: RepoOrder => OrderAction = o => CancelOrder(o.orderView, o.order)

  def createInsertOrder(desiredQty: Long, price: Double, buySell: Int, availableQty: Long): Option[OrderAction] =
    if (availableQty <= 0L) {
      None
    } else {
      val o = new Order()
      o.setPrice(price)
      o.setBuySell(buySell)
      o.setCustomField(CustomId.field, CustomId.generate.v)
      if (availableQty <= desiredQty) {
        o.setQuantity(availableQty)
      } else {
        o.setQuantity(desiredQty)
      }
      Some(InsertOrder(o))
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

  def updateLiveOrders(activeOrderDescriptorView: ActiveOrderDescriptorView, order: Order): F[Unit] =
    activeOrderDescriptorView.getAction match {
      case 1 | 2 => liveOrdersRepo.putOrder(underlyingSymbol, RepoOrder(activeOrderDescriptorView, order))
      case 3     => liveOrdersRepo.removeOrder(underlyingSymbol, order.getId)
      case _     => ().pure[F]
    }

  def process(preProcess: EitherT[F, Error, Order]): F[List[OrderAction]] =
    (for {
      b <- preProcess
      c <- EitherT.right[Error](createOrderActions(b))
      _ = logInfo(s"Algo 4. OrderAction: $c")
      d <- EitherT.right[Error](c.map(p => roundDownQty(p, lotSize).pure[F]).sequence)
      _ = logInfo(s"Algo 5. With rounded down qty: $d")
      e <- EitherT.rightT[F, Error](d.filter(removeZeroQty))
      _ = logInfo(s"Algo 6. After any order with qty = 0 is removed: $e")
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

  def handleOnOrderAck(
      activeOrderDescriptorView: ActiveOrderDescriptorView,
      preProcess: EitherT[F, Error, Order]
  ): EitherT[F, Error, Unit] =
    for {
      a <- EitherT.rightT[F, Error](activeOrderDescriptorView.getOrderCopy)
      customId = CustomId.fromOrder(a)
      _ <- EitherT.right[Error](updateLiveOrders(activeOrderDescriptorView, a))
      _ <- EitherT.right[Error](pendingOrdersRepo.remove(customId))
      d <- EitherT.right[Error](pendingCalculationRepo.shouldRecalculate)
      _ <- EitherT.right[Error](pendingCalculationRepo.deactivate())
      _ <- if (d) EitherT(handleOnSignal(preProcess)) else EitherT.rightT[F, Error](())
    } yield ()

  def handleOnOrderNak(
      customId: CustomId,
      errorMsg: String,
      preProcess: EitherT[F, Error, Order]
  ): EitherT[F, Error, Unit] =
    for {
      _ <- EitherT.rightT[F, Error](logAlert(errorMsg))
      _ <- EitherT.right[Error](pendingOrdersRepo.remove(customId))
      _ <- EitherT.right[Error](pendingCalculationRepo.deactivate())
    } yield ()
}

object Algo {
  def apply[F[_]: Applicative: Monad](
      underlyingSymbol: String,
      lotSize: Int,
      portfolioQty: Long,
      sendOrder: (OrderAction) => Order,
      logAlert: String => Unit,
      logInfo: String => Unit,
      logError: String => Unit
  ): F[Algo[F]] = {
    val liveOrdersRepo         = new LiveOrdersInMemInterpreter[F]
    val portfolioRepo          = new UnderlyingPortfolioInterpreter[F]
    val pendingOrdersRepo      = new PendingOrdersInMemInterpreter[F]
    val pendingCalculationRepo = new PendingCalculationInMemInterpreter[F]
    for {
      _ <- portfolioRepo.put(underlyingSymbol, Portfolio(underlyingSymbol, portfolioQty))
      b = new Algo(
        liveOrdersRepo,
        portfolioRepo,
        pendingOrdersRepo,
        pendingCalculationRepo,
        underlyingSymbol,
        lotSize,
        sendOrder,
        logAlert,
        logInfo,
        logError
      )
    } yield b
  }

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
}
