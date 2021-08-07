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
//val ulInstrument: InstrumentDescriptor
  /*
    define parameter val: Instrument

   */




  def calculate(): F[Order] = Monad[F].pure(
    createOrder(666L, 122.0, BuySell.BUY, UUID.randomUUID().toString)
  )

  def createOrderActions(order: Order): F[List[OrderAction]] = for {
    a <- liveOrdersRepo.getOrdersByTimeSortedDown(underlyingSymbol)
    b <- Monad[F].pure(calcTotalQty(a))
    c <- portfolioRepo.get(underlyingSymbol)
    d <- Monad[F].pure{
      if(a.isEmpty){
        List(createInsertOrderAgainstPortfolio(order, c, 0L))
      }
      else {
        if(order.getBuySell != a.head.getBuySell){
          a.map(createCancelOrder) :+ createInsertOrderAgainstPortfolio(order, c, b) // cancels then insert
        }
        else {
          if(order.getQuantityL == 0L){
            a.map(createCancelOrder)
          } else if (order.getQuantityL < b){
            trimLiveOrders(a, order.getQuantityL, ListBuffer.empty)
          } else {
            List(createInsertOrderAgainstPortfolio(order, c, b))
          }
        }
      }
    }
  } yield d

  def calcTotalQty(orders: List[Order]): Long =
    orders.foldLeft(0L)((a: Long, b: Order) => b.getBuySell match {
    case BuySell.BUY => a + b.getQuantityL
    case BuySell.SELL => a - b.getQuantityL
  })

  val createCancelOrder: Order => OrderAction = o => CancelOrder(o.getId)

  def createInsertOrderAgainstPortfolio(order: Order, p: Portfolio, livesQty: Long): OrderAction = {
    order.getBuySell match {
      case BuySell.BUY =>
        InsertOrder {
          val ava = p.position + livesQty
          if(order.getQuantityL > ava) {
            createOrder(ava, order.getPrice, BuySell.BUY, UUID.randomUUID().toString)
          } else {
            order
          }
        }
      case _ => InsertOrder(order)
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