package guardian

import cats.implicits._
import cats.{Id, Monad}
import com.ingalys.imc.BuySell
import com.ingalys.imc.order.Order
import guardian.Entities.OrderAction.{CancelOrder, UpdateOrder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.math.BigDecimal.RoundingMode

class AlgoTest extends AnyFlatSpec {

  //  behavior of "accumulateActiveOrders"

  //  it should "accumulate orders' quantities" in {
  //    val x = for {
  //      a <- Monad[Option].pure {
  //        val x = new LiveOrdersInMemInterpreter[Option]
  //        x.putOrder("aaa", Order("id1", 10L, 100, 50, Entities.BUY))
  //        x.putOrder("aaa", Order("id2", 11L, 20, 50, Entities.BUY))
  //        x
  //      }
  //      b <- Monad[Option].pure {
  //        new Application[Option](a)
  //      }
  //      c <- b.accumulateLiveOrders("aaa")
  //    } yield c
  //    x shouldBe Some(120)
  //  }

  // if new order's direction is different then Cancel ALL Live Orders
  // if live orders are empty or new quantity is bigger than accumulate live orders' quantity then Insert
  // else / if 0 < new quantity < accumulated liver orders' quantity then CancelOrder or UpdateOrder

  val symbol = "ptt"


  behavior of "trimLiveOrders"

  def createApp[F[_]: Monad](symbol: String, liveOrders: List[Order]): F[Algo[F]] =
    for {
      a <- Monad[F].pure {
        val x = new LiveOrdersInMemInterpreter[F]
        liveOrders.foreach(x.putOrder(symbol, _))
        x
      }
      b <- Monad[F].pure {
        new UnderlyingPortfolioInterpreter[F]
      }
      c <- Monad[F].pure {
        new PendingOrdersInMemInterpreter[F]
      }
      d <- Monad[F].pure {
        new PendingCalculationInMemInterpreter[F]
      }
      z <- Monad[F].pure {
        new Algo[F](a, b, c, d, symbol)
      }
    } yield z


  def createOrder(id: String, nanos: Long, qty: Long, price: Double, buySell: Int): Order = {
    val x = new Order()
    x.setId(id)
    x.setTimestampNanos(nanos) // assume we are the exchange who can assign timestamp
    x.setQuantity(qty)
    x.setPrice(price)
    x.setBuySell(buySell)
    x
  }

  it should "create 1 UpdateOrder if calQty between o2 and o3 in [o1, o2, o3]" in {
    val calQty = 135L
    val q1     = 100L
    val q2     = 30L
    val q3     = 10L
    val liveOrders = List(
      createOrder("id1", 10L, q1, 50.0, BuySell.BUY),
      createOrder("id2", 11L, q2, 50.0, BuySell.BUY),
      createOrder("id3", 12L, q3, 50.0, BuySell.BUY)
    )
    val x = for {
      a <- createApp[Option](symbol, liveOrders)
      b <- a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      _ = println(b)
      c <- a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.get.size shouldBe 1
    x.get.head.isInstanceOf[UpdateOrder] shouldBe true
    val y = x.get.head.asInstanceOf[UpdateOrder]
    y.order.getQuantityD shouldBe (calQty - q1 - q2)
  }

  it should "create 1 CancelOrder if calQty is equal to o1 + o2 in [o1, o2, o3] " in {
    val calQty = 130L
    val lastId = "id3"
    val liveOrders = List(
      createOrder("id1", 10L, 100L, 50.0, BuySell.BUY),
      createOrder("id2", 11L, 30L, 50.0, BuySell.BUY),
      createOrder(lastId, 12L, 10L, 50.0, BuySell.BUY)
    )
    val x = for {
      a <- createApp[Option](symbol, liveOrders)
      b <- a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c <- a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.get.size shouldBe 1
    x.get.head.isInstanceOf[CancelOrder] shouldBe true
    x.get.head.asInstanceOf[CancelOrder].id shouldBe lastId
  }

  it should "create two CancelOrder if calQty is equal to o1 in [o1, o2, o3]" in {
    val calQty = 100L
    val lastId = "id3"
    val id2    = "id2"
    val liveOrders = List(
      createOrder("id1", 10L, 100L, 50.0, BuySell.BUY),
      createOrder(id2, 11L, 30L, 50.0, BuySell.BUY),
      createOrder(lastId, 12L, 10L, 50.0, BuySell.BUY)
    )
    val x = for {
      a <- createApp[Option](symbol, liveOrders)
      b <- a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c <- a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.get.size shouldBe 2
    x.get.count(_.isInstanceOf[CancelOrder]) shouldBe 2
    x.get.head.asInstanceOf[CancelOrder].id shouldBe lastId
    x.get(1).asInstanceOf[CancelOrder].id shouldBe id2
  }

  it should "create 2 CancelOrders and 1 UpdateOrder if calQty is between 0 and o1 in [o1, o2, o3] " in {
    val calQty = 60L
    val id1    = "id1"
    val liveOrders = List(
      createOrder(id1, 10L, 100L, 50.0, BuySell.BUY),
      createOrder("id2", 11L, 30L, 50.0, BuySell.BUY),
      createOrder("id3", 12L, 10L, 50.0, BuySell.BUY)
    )
    val x = for {
      a <- createApp[Option](symbol, liveOrders)
      b <- a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c <- a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.get.size shouldBe 3
    x.get.head.isInstanceOf[CancelOrder] shouldBe true
    x.get(1).isInstanceOf[CancelOrder] shouldBe true
    x.get.last.isInstanceOf[UpdateOrder] shouldBe true
    val y = x.get.last.asInstanceOf[UpdateOrder]
    y.order.getQuantityL shouldBe (calQty - 0)
    y.order.getId shouldBe id1
  }

  it should "create 1 CancelOrders and 1 UpdateOrder if calQty is between o1 and o2 in [o1, o2, o3] " in {
    val calQty = 110L
    val id2    = "id2"
    val id3    = "id3"
    val q1     = 100L
    val liveOrders = List(
      createOrder("id1", 10L, 100L, 50.0, BuySell.BUY),
      createOrder(id2, 11L, 30L, 50.0, BuySell.BUY),
      createOrder(id3, 12L, 10L, 50.0, BuySell.BUY)
    )
    val x = for {
      a <- createApp[Option](symbol, liveOrders)
      b <- a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c <- a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.get.size shouldBe 2
    x.get.head.isInstanceOf[CancelOrder] shouldBe true
    x.get(1).isInstanceOf[UpdateOrder] shouldBe true
    val y = x.get.last.asInstanceOf[UpdateOrder]
    y.order.getQuantityL shouldBe (calQty - q1)
  }

  it should "cancel all orders when calQty = 0 " in {
    val calQty = 0L
    val id2    = "id2"
    val id3    = "id3"
    val liveOrders = List(
      createOrder("id1", 10L, 100L, 50.0, BuySell.BUY),
      createOrder(id2, 11L, 30L, 50.0, BuySell.BUY),
      createOrder(id3, 12L, 10L, 50.0, BuySell.BUY)
    )
    val x = for {
      a <- createApp[Option](symbol, liveOrders)
      b <- a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c <- a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.get.size shouldBe 3
    x.get.head.isInstanceOf[CancelOrder] shouldBe true
  }

  behavior of "getTotalQtyLiveOrders"

  it should "return positive sum of all BUY orders quantity all orders" in {
    val q1 = 100L
    val q2 = 30L
    val q3 = 10L
    val liveOrders = List(
      createOrder("id1", 10L, q1, 50.0, BuySell.BUY),
      createOrder("id2", 11L, q2, 50.0, BuySell.BUY),
      createOrder("id3", 12L, q3, 50.0, BuySell.BUY)
    )
    val x = for {
      a <- createApp[Id](symbol, liveOrders)
      c <- a.getTotalQtyLiveOrders(symbol)
    } yield c
    x shouldBe (q1 + q2 + q3)
  }

  it should "return negative sum of all SELL orders quantity all orders" in {
    val q1 = 100L
    val q2 = 30L
    val q3 = 10L
    val liveOrders = List(
      createOrder("id1", 10L, q1, 50.0, BuySell.SELL),
      createOrder("id2", 11L, q2, 50.0, BuySell.SELL),
      createOrder("id3", 12L, q3, 50.0, BuySell.SELL)
    )
    val x = for {
      a <- createApp[Id](symbol, liveOrders)
      c <- a.getTotalQtyLiveOrders(symbol)
    } yield c
    x shouldBe (q1 + q2 + q3) * -1
  }

  it should "return 0 if no live orders" in {
    val liveOrders = List()
    val x = for {
      a <- createApp[Id](symbol, liveOrders)
      c <- a.getTotalQtyLiveOrders(symbol)
    } yield c
    x shouldBe 0
  }

}





