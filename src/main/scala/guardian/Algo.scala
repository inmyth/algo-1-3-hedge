package guardian

import cats.Monad
import cats.effect.IO
import cats.implicits._
import com.ingalys.imc.BuySell
import com.ingalys.imc.order.Order
import guardian.Entities.{OrderAction, Portfolio}
import guardian.Entities.OrderAction.{CancelOrder, InsertOrder, UpdateOrder}
import horizontrader.services.instruments.InstrumentDescriptor

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import guardian.Algo._

class Algo[F[_]: Monad](
            val liveOrdersRepo: LiveOrdersRepoAlgebra[F],
            val portfolioRepo: UnderlyingPortfolioAlgebra[F],
            val pendingOrdersRepo: PendingOrdersAlgebra[F],
            val pendingCalculationRepo: PendingCalculationAlgebra[F],
            underlyingSymbol: String //PTT, Delta
          ) {

  def calculate(): F[Order] = Monad[F].pure(
    createOrder(666L, 122.0, BuySell.BUY, UUID.randomUUID().toString)
  )

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

  def updateLiveOrders(act: OrderAction, symbol: String): F[Unit] =
    act match {
      case InsertOrder(order) => liveOrdersRepo.putOrder(symbol, order)
      case UpdateOrder(order) => liveOrdersRepo.putOrder(symbol, order)
      case CancelOrder(id)    => liveOrdersRepo.removeOrder(symbol, id)
    }

  def process(): F[List[OrderAction]] = for {
    a <- calculate()
    b <- createOrderActions(a)
    _ <- b.map(p => pendingOrdersRepo.put(p)).sequence
  } yield b


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