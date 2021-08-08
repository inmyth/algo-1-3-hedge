package guardian

import algotrader.api.source.Source
import cats.Monad
import cats.data.EitherT
import cats.implicits._
import com.ingalys.imc.BuySell
import com.ingalys.imc.order.Order
import com.ingalys.imc.summary.Summary
import guardian.Algo._
import guardian.Entities.{DwData, OrderAction}
import guardian.Entities.OrderAction.{CancelOrder, InsertOrder, UpdateOrder}
import guardian.Error.{MarketError, StateError, UnknownError}
import horizontrader.services.instruments.InstrumentDescriptor

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

class Algo[F[_]: Monad](
            val liveOrdersRepo: LiveOrdersRepoAlgebra[F],
            val portfolioRepo: UnderlyingPortfolioAlgebra[F],
            val pendingOrdersRepo: PendingOrdersAlgebra[F],
            val pendingCalculationRepo: PendingCalculationAlgebra[F],
            underlyingSymbol: String, //PTT, Delta
            dwMap: Map[String, Source[Summary]],
            sendOrder: (String, Double) => Unit
          ) {

  def calculateOrder(dwData: DwData): F[Order] = Monad[F].pure(

    createOrder(666L, 122.0, BuySell.BUY, UUID.randomUUID().toString)
  )

  def getDWData(id: String): EitherT[F, Error, DwData] = for {
    a <- EitherT.fromEither[F](dwMap.get(id).fold[Either[Error, Source[Summary]]](Left(MarketError(s"DW Market not available, id:$id")))(p => Right(p)))
    b <- EitherT.fromEither[F](a.latest.fold[Either[Error, DwData]](Left(MarketError(s"DW latest data not available, id:$id")))(p => {Right(DwData(10L, 20.0))}))
  } yield b


  def createOrderActions(order: Order): F[List[OrderAction]] = for {
    a <- liveOrdersRepo.getOrdersByTimeSortedDown(underlyingSymbol)
    b <- Monad[F].pure(calcTotalQty(a))
    c <- portfolioRepo.get(underlyingSymbol)
    d <- Monad[F].pure{
      if(a.isEmpty){
        createInsertOrder(order.getQuantityL, order.getPrice, order.getBuySell, c.position).toList
      }
      else {
        if(order.getBuySell != a.head.getBuySell){
          a.map(createCancelOrder) ++  createInsertOrder(order.getQuantityL, order.getPrice, order.getBuySell, c.position).toList // cancels are first
        }
        else {
          val wantQty = order.getQuantityL - b
          if (wantQty < 0L){
            trimLiveOrders(a, order.getQuantityL, ListBuffer.empty)
          }
          else if(wantQty == 0L){
            a.map(createCancelOrder)
          }
          else {
            val avaQty = if(order.getBuySell == BuySell.BUY) Long.MaxValue else c.position - b
            createInsertOrder(wantQty, order.getPrice, order.getBuySell, avaQty).toList
          }
        }
      }
    }
  } yield d

  def calcTotalQty(orders: List[Order]): Long = orders.foldLeft(0L)((a: Long, b: Order) => a + b.getQuantityL)

  val createCancelOrder: Order => OrderAction = o => CancelOrder(o.getId)

  def createInsertOrder(wantQty: Long, price: Double, buySell: Int, availableQty: Long): Option[OrderAction] = {
    buySell match {
      case BuySell.SELL =>
          if(availableQty <= 0L){
            None
          } else if(availableQty <= wantQty) {
            Some(InsertOrder(createOrder(availableQty, price, buySell, UUID.randomUUID().toString)))
          } else {
            Some(InsertOrder(createOrder(wantQty, price, buySell, UUID.randomUUID().toString)))
          }
      case _ => Some(InsertOrder(createOrder(wantQty, price, buySell, UUID.randomUUID().toString)))
    }
  }

  @tailrec
  final def trimLiveOrders(
                            liveOrders: List[Order],
                            calculatedQuantity: Long,
                            res: ListBuffer[OrderAction]
                          ):List[OrderAction] = {
    val head :: rest = liveOrders
    val liveQty      = rest.foldLeft(0L)((x: Long, y: Order) => x + y.getQuantityL)
    if (liveQty < calculatedQuantity) {
      val remainder = calculatedQuantity - liveQty
      val o = createOrder(remainder,head.getPrice, head.getBuySell, head.getId )
      res += UpdateOrder(o)
      res.toList
    } else if (liveQty == calculatedQuantity) {
      res += CancelOrder(head.getId)
      res.toList
    } else {
      res += CancelOrder(head.getId)
      trimLiveOrders(rest, calculatedQuantity, res)
    }
  }

  def updateLiveOrders(act: OrderAction): F[Unit] =
    act match {
      case InsertOrder(order) => liveOrdersRepo.putOrder(underlyingSymbol, order)
      case UpdateOrder(order) => liveOrdersRepo.putOrder(underlyingSymbol, order)
      case CancelOrder(id)    => liveOrdersRepo.removeOrder(underlyingSymbol, id)
    }

  def sendOrderAction(act: OrderAction): F[Unit] = Monad[F].pure{
    sendOrder("a", 2.0)
  }

  def process(dwId: String): F[List[OrderAction]] = (for {
    a <- getDWData(dwId)
    b <- EitherT.right[Error](calculateOrder(a))
    c <- EitherT.right[Error](createOrderActions(b))
  } yield c).value.map{
    case Right(v) => v
    case Left(e) =>
      println(e)
      List.empty[OrderAction]
  }


  def getPendingOrderAction(dwId: String): EitherT[F, Error, OrderAction] = for {
    a <- EitherT.liftF[F, Error, Option[OrderAction]](pendingOrdersRepo.get(dwId))
    b <- EitherT.fromEither[F](a.fold[Either[Error, OrderAction]](Left(UnknownError(s"Pending order not found, dwId:$dwId")))(p => Right(p)))
  } yield b

  def checkPendingCalculations(dwId: String): EitherT[F, StateError, Unit] = for {
    a <- EitherT.right(pendingCalculationRepo.shouldCalculate(dwId))
    b <- EitherT(Monad[F].pure(if(a) Right(()) else Left(StateError(s"Cannot procees, there are order pending for market id: $dwId"))))
  } yield b

  def handleOnSignal(dwId: String) = (for {
    _ <- checkPendingCalculations(dwId)
    a <- EitherT.right[Error](process(dwId))
    _ <- EitherT.right[Error](a.map(sendOrderAction).sequence)
    _ <- EitherT.right[Error](a.map(pendingOrdersRepo.put).sequence)
  } yield dwId).value.map{
    case Right(v) => Some(v)
    case Left(e) =>
      println(e.msg)
      None
  }


  def handleOnOrderAck(id: String) =
    (for {
    a <- getPendingOrderAction(id)
    _ <- EitherT.right[Error](pendingOrdersRepo.remove(id))
    _ <- EitherT.right[Error](updateLiveOrders(a))
    d <- EitherT.right[Error](pendingCalculationRepo.getAll)
    e <- EitherT.right[Error](d.map(handleOnSignal).sequence)
    _ <- EitherT.right[Error](e.filter(_.isDefined).map(p => pendingCalculationRepo.remove(p.get)).sequence)
  } yield ())

  def handleOnOrderNak(id: String, errorMsg: String) =
    (for {
      _ <- EitherT.right[Error](Monad[F].pure(
        println(errorMsg)
      ))
      _ <- EitherT.right[Error](pendingOrdersRepo.remove(id))
      d <- EitherT.right[Error](pendingCalculationRepo.getAll)
      e <- EitherT.right[Error](d.map(handleOnSignal).sequence)
      _ <- EitherT.right[Error](e.filter(_.isDefined).map(p => pendingCalculationRepo.remove(p.get)).sequence)
    } yield ())

}

object Algo{
  def createOrder(qty: Long, prc: Double, buysell: Int, id: String): Order = {
    val o = new Order()
    o.setQuantity(qty)
    o.setPrice(prc)
    o.setBuySell(buysell)
    o.setId(id)
    o
  }
}