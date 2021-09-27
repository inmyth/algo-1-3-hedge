package guardian

import cats.data.EitherT
import cats.implicits._
import cats.{Applicative, Id, Monad}
import com.ingalys.imc.BuySell
import com.ingalys.imc.order.Order
import guardian.Algo._
import guardian.Entities.OrderAction.{CancelOrder, InsertOrder, UpdateOrder}
import guardian.Entities.PutCall.{CALL, PUT}
import guardian.Entities.{CustomId, Direction, OrderAction, Portfolio, PutCall}
import guardian.Error.UnknownError
import horizontrader.plugins.hmm.connections.service.IDictionaryProvider
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
    preProcess: EitherT[F, Error, Order],
    sendOrder: (OrderAction) => Order,
    logAlert: String => Unit,
    logInfo: String => Unit,
    logError: String => Unit
) {
  def createOrderActions(order: Order): F[List[OrderAction]] =
    for {
      a <- liveOrdersRepo.getOrdersByTimeSortedDown(underlyingSymbol)
      b <- Monad[F].pure(calcTotalQty(a))
      c <- portfolioRepo.get(underlyingSymbol)
      d <- Monad[F].pure {
        if (a.isEmpty) {
          createInsertOrder(order.getQuantityL, order.getPrice, order.getBuySell, c.position).toList
        } else {
          if (order.getBuySell != a.head.getBuySell) {
            a.map(createCancelOrder) ++ createInsertOrder(
              order.getQuantityL,
              order.getPrice,
              order.getBuySell,
              c.position
            ).toList // cancels are first
          } else {
            val wantQty = order.getQuantityL - b
            if (wantQty < 0L) {
              trimLiveOrders(a, order.getQuantityL, ListBuffer.empty)
            } else if (wantQty == 0L) {
              a.map(createCancelOrder)
            } else {
              val avaQty = if (order.getBuySell == BuySell.BUY) Long.MaxValue else c.position - b
              createInsertOrder(wantQty, order.getPrice, order.getBuySell, avaQty).toList
            }
          }
        }
      }
    } yield d

  def calcTotalQty(orders: List[Order]): Long = orders.foldLeft(0L)((a: Long, b: Order) => a + b.getQuantityL)

  val createCancelOrder: Order => OrderAction = o => CancelOrder(o)

  def createInsertOrder(wantQty: Long, price: Double, buySell: Int, availableQty: Long): Option[OrderAction] = {
    buySell match {
      case BuySell.SELL =>
        if (availableQty <= 0L) {
          None
        } else if (availableQty <= wantQty) {
          Some(InsertOrder(createOrder(availableQty, price, buySell, CustomId.generate)))
        } else {
          Some(InsertOrder(createOrder(wantQty, price, buySell, CustomId.generate)))
        }
      case _ => Some(InsertOrder(createOrder(wantQty, price, buySell, CustomId.generate)))
    }
  }

  @tailrec
  final def trimLiveOrders(
      liveOrders: List[Order],
      calculatedQuantity: Long,
      res: ListBuffer[OrderAction]
  ): List[OrderAction] = {
    val head :: rest = liveOrders
    val liveQty      = rest.foldLeft(0L)((x: Long, y: Order) => x + y.getQuantityL)
    if (liveQty < calculatedQuantity) {
      val remainder = calculatedQuantity - liveQty
      val o         = cloneModifyOrder(head, remainder, head.getPrice, head.getBuySell)
      res += UpdateOrder(o)
      res.toList
    } else if (liveQty == calculatedQuantity) {
      res += CancelOrder(head)
      res.toList
    } else {
      res += CancelOrder(head)
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

  def updateLiveOrders(act: OrderAction): F[Unit] =
    act match {
      case InsertOrder(order) => liveOrdersRepo.putOrder(underlyingSymbol, order)
      case UpdateOrder(order) => liveOrdersRepo.putOrder(underlyingSymbol, order)
      case CancelOrder(order) => liveOrdersRepo.removeOrder(underlyingSymbol, order.getId)
    }

  def process(): F[List[OrderAction]] =
    (for {
      b <- preProcess
      c <- EitherT.right[Error](createOrderActions(b))
    } yield c).value.map {
      case Right(v) => v
      case Left(e) =>
        logAlert(e.msg)
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
  def handleOnSignal(): F[Either[Error, Unit]] =
    (for {
      _ <- checkAndUpdatePendingOrderAndCalculation()
      a <- EitherT.right[Error](process())
      _ <- EitherT.right[Error](a.map(p => Monad[F].pure(sendOrder(p))).sequence)
      _ <- EitherT.right[Error](a.map(pendingOrdersRepo.put).sequence)
    } yield ()).value.map {
      case Right(_) => Right(())
      case Left(e) =>
        logAlert(e.msg)
        Left(e)
    }

  def handleOnOrderAck(customId: CustomId): EitherT[F, Error, Unit] =
    for {
      a <- getPendingOrderAction(customId)
      _ <- EitherT.right[Error](updateLiveOrders(a))
      _ <- EitherT.right[Error](pendingOrdersRepo.remove(customId))
      d <- EitherT.right[Error](pendingCalculationRepo.shouldRecalculate)
      _ <- if (d) EitherT.liftF(handleOnSignal()) else EitherT.rightT[F, Error](())
      _ <- EitherT.right[Error](pendingCalculationRepo.deactivate())
    } yield ()

  def handleOnOrderNak(customId: CustomId, errorMsg: String): EitherT[F, Error, Unit] =
    for {
      _ <- EitherT.right[Error](
        Monad[F].pure(
          logAlert(errorMsg)
        )
      )
      _ <- handleOnOrderAck(customId)
    } yield ()

  // second init on Load signal
  def handleOnLoad(ulId: String, qty: Long) =
    for {
      _ <- EitherT.right[Error](portfolioRepo.put(ulId, Portfolio(ulId, qty)))
    } yield ()

}

object Algo {

  def xxx[F[_]: Monad: Applicative](bbb: String => Unit) = Monad[F].pure(bbb("adsadasdsads"))

  def xxx2(bbb: String => Unit) = xxx[Id](bbb)

  def xxx10[F[_]: Monad: Applicative](bbb: String) = Monad[F].pure(bbb)

  def xxx3(v: InstrumentDescriptor) = xxx10[Id](v.getUniqueId)

  def build(
      underlyingSymbol: String,
      sendOrder: (OrderAction) => Order,
      logAlert: String => Unit,
      logInfo: String => Unit,
      logError: String => Unit
  ): Algo[Id] =
    Algo(
      liveOrdersRepo = new LiveOrdersInMemInterpreter[Id](),
      portfolioRepo = new UnderlyingPortfolioInterpreter[Id](),
      pendingOrdersRepo = new PendingOrdersInMemInterpreter[Id](),
      pendingCalculationRepo = new PendingCalculationInMemInterpreter[Id](),
      underlyingSymbol = underlyingSymbol,
      preProcess = EitherT.leftT(Error.MarketError("aaaa")),
      sendOrder = sendOrder,
      logAlert = logAlert,
      logInfo = logInfo,
      logError = logError
    )

  def apply[F[_]: Applicative: Monad](
      liveOrdersRepo: LiveOrdersRepoAlgebra[F],
      portfolioRepo: UnderlyingPortfolioAlgebra[F],
      pendingOrdersRepo: PendingOrdersAlgebra[F],
      pendingCalculationRepo: PendingCalculationAlgebra[F],
      underlyingSymbol: String,
      preProcess: EitherT[F, Error, Order],
      sendOrder: (OrderAction) => Order,
      logAlert: String => Unit,
      logInfo: String => Unit,
      logError: String => Unit
  ): Algo[F] =
    new Algo(
      liveOrdersRepo,
      portfolioRepo,
      pendingOrdersRepo,
      pendingCalculationRepo,
      underlyingSymbol,
      preProcess,
      sendOrder,
      logAlert,
      logInfo,
      logError
    )

  def createOrder(qty: Long, prc: Double, buysell: Int, customId: CustomId): Order = {
    val o = new Order()
    o.setQuantity(qty)
    o.setPrice(prc)
    o.setBuySell(buysell)
    o.setCustomField(CustomId.field, customId.v)
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

  def preProcess[F[_]: Monad: Applicative](
      getDwList: (IDictionaryProvider, String, String) => List[InstrumentDescriptor],
      getDwProjectedPrice: InstrumentDescriptor => Option[Double],
      getOwnBestAskPrice: InstrumentDescriptor => Option[Double],
      calcUlQtyPreResidual: (Double, Double, Double, Double, String, String) => Long,
      getPutOrCall: InstrumentDescriptor => Option[PutCall],
      getUlProjectedPrice: (InstrumentDescriptor, Direction) => Either[Error, BigDecimal],
      getDelta: String => Option[Double],
      getPointValue: InstrumentDescriptor => Double,
      getAbsoluteResidual: (Double, String) => Option[BigDecimal],
      ulInstrument: InstrumentDescriptor,
      dictionaryService: IDictionaryProvider,
      hedgeInstrument: InstrumentDescriptor,
      context: String = "DEFAULT",
      exchangeName: String = "SET"
  ): EitherT[F, Error, Order] =
    for {
      ulId                 <- EitherT.rightT[F, Error](ulInstrument.getUniqueId)
      dwList               <- EitherT.rightT[F, Error](getDwList(dictionaryService, ulId, exchangeName))
      dwProjectedPriceList <- EitherT.rightT[F, Error](dwList.map(getDwProjectedPrice))
      bestBidPriceList     <- EitherT.rightT[F, Error](dwList.map(getDwProjectedPrice))
      bestAskPriceList     <- EitherT.rightT[F, Error](dwList.map(getOwnBestAskPrice))
      deltaList            <- EitherT.rightT[F, Error](dwList.map(p => getDelta(p.getUniqueId)))
      dwPutCallList        <- EitherT.rightT[F, Error](dwList.map(getPutOrCall))
      pointValue           <- EitherT.rightT[F, Error](getPointValue(hedgeInstrument))
      absoluteResidual     <- EitherT.rightT[F, Error](getAbsoluteResidual(pointValue, ulId))
      signedDeltaList <- EitherT.rightT[F, Error](
        (deltaList, dwPutCallList).zipped.toList
          .map {
            case (Some(delta), Some(CALL)) => 1 * delta
            case (Some(delta), Some(PUT))  => -1 * delta
            case _                         => 0
          }
      )
      partialResidual <- EitherT.rightT[F, Error](
        (bestBidPriceList, bestAskPriceList, dwProjectedPriceList).zipped.toList
          .zip(signedDeltaList)
          .map {
            case ((Some(a), Some(b), Some(c)), d) => (a, b, c, d)
            case ((_, _, _), d)                   => (0.0d, 0.0d, 0.0d, d)
          }
          .zip(dwList)
          .map {
            case ((a, b, c, d), e) => (a, b, c, d, e.getUniqueId)
          }
          .map(p => calcUlQtyPreResidual(p._1, p._2, p._3, p._4, p._5, context))
          .sum
      )
      totalResidual = partialResidual + absoluteResidual.getOrElse(BigDecimal("0")).toLong
      direction     = if (totalResidual < 0) Direction.SELL else Direction.BUY
      hzDirection   = if (direction == Direction.SELL) BuySell.SELL else BuySell.BUY
      ulProjectedPrice <- EitherT.fromEither[F](getUlProjectedPrice(ulInstrument, direction))
      absTotalResidual = Math.abs(totalResidual)
      order            = Algo.createOrder(absTotalResidual, ulProjectedPrice.toDouble, hzDirection, CustomId.generate)
      _ <- EitherT.rightT[F, Error](
        Either.cond(order.getQuantityL > 0, (), Error.StateError("Pre-process order qty cannot be negative"))
      )
    } yield order

}
