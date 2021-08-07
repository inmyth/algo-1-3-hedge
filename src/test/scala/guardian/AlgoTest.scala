package guardian

import cats.implicits._
import cats.{Id, Monad}
import com.ingalys.imc.BuySell
import guardian.Entities.OrderAction.{CancelOrder, UpdateOrder}
import guardian.Shared.createOrder
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable.ListBuffer
import scala.language.{higherKinds, postfixOps}

class AlgoTest extends AnyFlatSpec {

  val symbol = "ptt"

  behavior of "trimLiveOrders"

  def createApp[F[_]: Monad](symbol: String): Algo[F] = {
      val a = new LiveOrdersInMemInterpreter[F]
      val b = new UnderlyingPortfolioInterpreter[F]
      val c = new PendingOrdersInMemInterpreter[F]
      val d = new PendingCalculationInMemInterpreter[F]
      new Algo[F](a, b, c, d, symbol)
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
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      b = a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c = a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.size shouldBe 1
    x.head.isInstanceOf[UpdateOrder] shouldBe true
    val y = x.head.asInstanceOf[UpdateOrder]
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
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      b = a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c = a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.size shouldBe 1
    x.head.isInstanceOf[CancelOrder] shouldBe true
    x.head.asInstanceOf[CancelOrder].id shouldBe lastId
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
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      b = a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c = a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.size shouldBe 2
    x.count(_.isInstanceOf[CancelOrder]) shouldBe 2
    x.head.asInstanceOf[CancelOrder].id shouldBe lastId
    x(1).asInstanceOf[CancelOrder].id shouldBe id2
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
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      b = a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c = a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.size shouldBe 3
    x.head.isInstanceOf[CancelOrder] shouldBe true
    x(1).isInstanceOf[CancelOrder] shouldBe true
    x.last.isInstanceOf[UpdateOrder] shouldBe true
    val y = x.last.asInstanceOf[UpdateOrder]
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
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      b = a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c = a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.size shouldBe 2
    x.head.isInstanceOf[CancelOrder] shouldBe true
    x(1).isInstanceOf[UpdateOrder] shouldBe true
    val y = x.last.asInstanceOf[UpdateOrder]
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
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      b = a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c = a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.size shouldBe 3
    x.head.isInstanceOf[CancelOrder] shouldBe true
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
    val a = createApp[Id](symbol)
    val b = a.calcTotalQty(liveOrders)
    b shouldBe (q1 + q2 + q3)
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
    val a = createApp[Id](symbol)
    val b = a.calcTotalQty(liveOrders)
    b shouldBe (q1 + q2 + q3) * -1
  }

  it should "return 0 if no live orders" in {
    val liveOrders = List()
    val a = createApp[Id](symbol)
    val b = a.calcTotalQty(liveOrders)
    b shouldBe 0
  }

}





