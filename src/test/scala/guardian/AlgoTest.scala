package guardian

import cats.data.EitherT
import cats.implicits._
import cats.{Id, Monad}
import com.ingalys.imc.BuySell
import guardian.Entities.OrderAction.{CancelOrder, InsertOrder, UpdateOrder}
import guardian.Entities.{CustomId, OrderAction, Portfolio}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import guardian.Fixtures.{lastId, _}

import scala.collection.mutable.ListBuffer
import scala.language.{higherKinds, postfixOps}

class AlgoTest extends AnyFlatSpec {

  val symbol = "ptt"

  behavior of "trimLiveOrders"

  def createApp[F[_]: Monad](symbol: String): Algo[F] = {
    new Algo(
      liveOrdersRepo = new LiveOrdersInMemInterpreter[F],
      portfolioRepo = new UnderlyingPortfolioInterpreter[F],
      pendingOrdersRepo = new PendingOrdersInMemInterpreter[F],
      pendingCalculationRepo = new PendingCalculationInMemInterpreter[F],
      underlyingSymbol = symbol,
      preProcess = EitherT.fromEither(Right(liveOrders.head)),
      sendOrder = (_: OrderAction) => liveOrders.head,
      logAlert = (s: String) => (println(s))
    )
  }

  it should "create 1 UpdateOrder if calQty between o2 and o3 in [o1, o2, o3]" in {
    val calQty = 135L
    val q1     = 100L
    val q2     = 30L
    val q3     = 10L
    val liveOrders = List(
      createTestOrder(id1, 10L, q1, 50.0, BuySell.BUY, customId1),
      createTestOrder(id2, 11L, q2, 50.0, BuySell.BUY, customId2),
      createTestOrder(lastId, 12L, q3, 50.0, BuySell.BUY, customId3)
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
    val liveOrders = List(
      createTestOrder(id1, 10L, 100L, 50.0, BuySell.BUY, customId1),
      createTestOrder(id2, 11L, 30L, 50.0, BuySell.BUY, customId2),
      createTestOrder(lastId, 12L, 10L, 50.0, BuySell.BUY, customId3)
    )
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      b = a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c = a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.size shouldBe 1
    x.head.isInstanceOf[CancelOrder] shouldBe true
    x.head.asInstanceOf[CancelOrder].order.getId shouldBe lastId
  }

  it should "create two CancelOrder if calQty is equal to o1 in [o1, o2, o3]" in {
    val calQty = 100L
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      b = a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c = a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.size shouldBe 2
    x.count(_.isInstanceOf[CancelOrder]) shouldBe 2
    x.head.asInstanceOf[CancelOrder].order.getId shouldBe lastId
    x(1).asInstanceOf[CancelOrder].order.getId shouldBe id2
  }

  it should "create 2 CancelOrders and 1 UpdateOrder if calQty is between 0 and o1 in [o1, o2, o3] " in {
    val calQty = 60L
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
    val q1     = 100L
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
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      b = a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
      c = a.trimLiveOrders(b, calQty, ListBuffer.empty)
    } yield c
    x.size shouldBe 3
    x.head.isInstanceOf[CancelOrder] shouldBe true
  }

  behavior of "createOrderActions"

  it should "return InsertOrder when buying and live orders are empty" in {
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      c <- a.createOrderActions(rawOrderBuy)
    } yield c
    x.size shouldBe 1
    x.head.isInstanceOf[InsertOrder] shouldBe true
  }

  it should "return InsertOrder when selling and live orders are empty and portfolio has enough position" in {
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ <- a.portfolioRepo.put(symbol, Portfolio(symbol, Long.MaxValue))
      c <- a.createOrderActions(rawOrderSell)
    } yield c
    x.size shouldBe 1
    x.head.isInstanceOf[InsertOrder] shouldBe true
  }

  it should "return only CancelOrders when selling if its direction and live orders' direction don't match and portfolio is empty" in {
    val liveOrders = List(
      createTestOrder(id1, 10L, 100L, 50.0, BuySell.BUY, customId1),
      createTestOrder(id2, 11L, 30L, 50.0, BuySell.BUY, customId2),
      createTestOrder(lastId, 12L, 10L, 50.0, BuySell.BUY, customId3)
    )
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      c <- a.createOrderActions(rawOrderSell)
    } yield c
    x.size shouldBe liveOrders.size
  }
  /*
  sell, qty larger than live orders, portfolio enough
  sell, qty larger than live orders, portfolio not enough
  sell, qty smaller than live orders, portfolio enough
  sell, qty smaller than live orders, portfolio not enough
   */
  it should "return one InsertOrder when selling with not enough position, new qty being current position = portfolio - live orders qty" in {
    val qty1     = 100L
    val qty2     = 30L
    val qty3     = 10L
    val position = 200L
    val liveOrders = List(
      createTestOrder(id1, 10L, qty1, 50.0, BuySell.SELL, customId1),
      createTestOrder(id2, 11L, qty2, 50.0, BuySell.SELL, customId2),
      createTestOrder(lastId, 12L, qty3, 50.0, BuySell.SELL, customId3)
    )
    val portfolio = Portfolio(symbol, position)
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      _ <- a.portfolioRepo.put(symbol, portfolio)
      c <- a.createOrderActions(rawOrderSell)
    } yield c
    x.size shouldBe 1
    x.head.asInstanceOf[InsertOrder].order.getQuantityL shouldBe position - (qty1 + qty2 + qty3)
  }

  it should "return one InsertOrder when buying with not enough position (buy doesn't depend on portfolio), new qty = order.qty - live orders qty" in {
    val qty1     = 100L
    val qty2     = 30L
    val qty3     = 10L
    val position = 200L
    val liveOrders = List(
      createTestOrder(id1, 10L, qty1, 50.0, BuySell.BUY, customId1),
      createTestOrder(id2, 11L, qty2, 50.0, BuySell.BUY, customId2),
      createTestOrder(lastId, 12L, qty3, 50.0, BuySell.BUY, customId3)
    )
    val portfolio = Portfolio(symbol, position)
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      _ <- a.portfolioRepo.put(symbol, portfolio)
      c <- a.createOrderActions(rawOrderBuy)
    } yield c
    x.size shouldBe 1
    x.head.asInstanceOf[InsertOrder].order.getQuantityL shouldBe rawOrderBuy.getQuantityL - (qty1 + qty2 + qty3)
  }

  it should "return one InsertOrder when selling with enough position, new qty = order.qty - live orders qty" in {
    val qty1     = 100L
    val qty2     = 30L
    val qty3     = 10L
    val position = 1000000L
    val liveOrders = List(
      createTestOrder(id1, 10L, qty1, 50.0, BuySell.SELL, customId1),
      createTestOrder(id2, 11L, qty2, 50.0, BuySell.SELL, customId2),
      createTestOrder(lastId, 12L, qty3, 50.0, BuySell.SELL, customId3)
    )
    val portfolio = Portfolio(symbol, position)
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      _ <- a.portfolioRepo.put(symbol, portfolio)
      c <- a.createOrderActions(rawOrderSell)
    } yield c
    x.size shouldBe 1
    x.head.asInstanceOf[InsertOrder].order.getQuantityL shouldBe rawOrderSell.getQuantityL - (qty1 + qty2 + qty3)
  }

  it should "trim live orders when sell or buy order qty is smaller than live orders qty" in {
    val liveOrders = List(
      createTestOrder(id1, 10L, 100L, 50.0, BuySell.SELL, customId1),
      createTestOrder(id2, 11L, 30L, 50.0, BuySell.SELL, customId2),
      createTestOrder(lastId, 12L, 10L, 50.0, BuySell.SELL, customId3)
    )
    val portfolio = Portfolio(symbol, 200L)
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      _ <- a.portfolioRepo.put(symbol, portfolio)
      c <- a.createOrderActions(createTestOrder("xx", 10L, 110, 50.0, BuySell.SELL, CustomId.generate))
    } yield c
    x.size shouldBe 2
    x.head.isInstanceOf[CancelOrder] shouldBe true
    x.last.isInstanceOf[UpdateOrder] shouldBe true
    x.last.asInstanceOf[UpdateOrder].order.getQuantityL shouldBe 10L
  }

  it should "cancel all live orders when sell or buy order qty is 0" in {
    val liveOrders = List(
      createTestOrder(id1, 10L, 100L, 50.0, BuySell.SELL, customId1),
      createTestOrder(id2, 11L, 30L, 50.0, BuySell.SELL, customId2),
      createTestOrder(lastId, 12L, 10L, 50.0, BuySell.SELL, customId3)
    )
    val portfolio = Portfolio(symbol, 200L)
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
      _ <- a.portfolioRepo.put(symbol, portfolio)
      c <- a.createOrderActions(createTestOrder("xx", 10L, 0, 50.0, BuySell.SELL, CustomId.generate))
    } yield c
    x.count(_.isInstanceOf[CancelOrder]) shouldBe liveOrders.size
  }

  behavior of "checkPendingOrderAction"

  it should "return Left when it pending orders exist " in {
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ <- a.pendingOrdersRepo.put(InsertOrder(rawOrderBuy))
      c <- a.checkPendingOrderAction()
    } yield c
    x.isLeft shouldBe true
  }

  it should "return Right when it pending orders empty " in {
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      c <- a.checkPendingOrderAction()
    } yield c
    x.isRight shouldBe true
  }

  behavior of "integrated test: handleOnSignal"

  val dw1 = "PTT@ABC"

  it should "return Left if pendingOrders is not empty" in {
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ <- a.pendingOrdersRepo.put(InsertOrder(rawOrderBuy))
      c <- a.handleOnSignal(dw1)
    } yield c
    x.isLeft shouldBe true
  }

  it should "return Right if pendingComputation is empty" in {
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      b <- a.handleOnSignal(dw1)
    } yield b
    x.isRight shouldBe true
  }

  behavior of "integrated test: handleOnOrderAck"

  it should "return Right if pendingOrder is available in the repo" in {
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      _ <- a.pendingOrdersRepo.put(InsertOrder(rawOrderBuy))
      c = a.handleOnOrderAck(CustomId.fromOrder(rawOrderBuy))
    } yield c
    x.value.isRight shouldBe true
  }

  it should "return Left if pendingOrder is not available in the repo" in {
    val x = for {
      a <- Monad[Id].pure(createApp[Id](symbol))
      c = a.handleOnOrderAck(CustomId.fromOrder(rawOrderBuy))
    } yield c
    x.value.isLeft shouldBe true
  }

//  behavior of "integrated test: handleOnOrderNak"

  behavior of "getPriceAfterTicks"

  it should "return 32 when price is 30.75 ticked down 5 steps" in {
    Algo.getPriceAfterTicks(false, BigDecimal(32), 5) shouldBe 30.75
  }

  it should "return 24.70 when price is 25.50 ticked down 5 steps" in {
    Algo.getPriceAfterTicks(false, BigDecimal(25.50), 5) shouldBe 24.70
  }

  it should "return 101.5 when price is 99.5 ticked up 5 steps" in {
    Algo.getPriceAfterTicks(true, BigDecimal(99.50), 5) shouldBe 101.5
  }

  it should "return 98.25 when price is 99.5 ticked down 5 steps" in {
    Algo.getPriceAfterTicks(false, BigDecimal(99.50), 5) shouldBe 98.25
  }

}
