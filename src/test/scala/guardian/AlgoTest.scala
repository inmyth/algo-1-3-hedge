package guardian

import cats.data.EitherT
import cats.implicits._
import cats.{Id, Monad}
import com.ingalys.imc.BuySell
import com.ingalys.imc.order.Order
import guardian.Entities.OrderAction.{CancelOrder, InsertOrder, UpdateOrder}
import guardian.Entities.{CustomId, OrderAction, Portfolio}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import guardian.Fixtures.{lastId, _}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable.ListBuffer
import scala.language.{higherKinds, postfixOps}

class AlgoTest extends AnyWordSpec with Matchers {

  def createApp[F[_]: Monad](): Algo[F] = {
    new Algo(
      liveOrdersRepo = new LiveOrdersInMemInterpreter[F],
      portfolioRepo = new UnderlyingPortfolioInterpreter[F],
      pendingOrdersRepo = new PendingOrdersInMemInterpreter[F],
      pendingCalculationRepo = new PendingCalculationInMemInterpreter[F],
      underlyingSymbol = symbol,
      lotSize = lotSize,
      sendOrder = (_: OrderAction) => liveBuyOrders.head,
      logAlert = (s: String) => println(s),
      logInfo = (s: String) => println(s),
      logError = (s: String) => println(s)
    )
  }

  def baseProcess(liveOrders: List[Order], portfolioQty: Long): Order => Id[List[OrderAction]] =
    (order: Order) =>
      for {
        a <- createApp[Id]().pure[Id]
        _ <- a.portfolioRepo.put(symbol, Portfolio(symbol, portfolioQty))
        _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
        c <- a.process(EitherT.fromEither(order.asRight[Error]))
      } yield c

  "process" when {
    "portfolio has enough position" when {
      "there are three orders on BUY side with quantity increasing along time" should {
        val process = baseProcess(liveBuyOrders, portfolioQty)
        "with enough qty in portfolio, create an UpdateOrder for o3" when {
          "calculated quantity is more than o1 + o2 but less than o1 + o2 + o3 " in {
            val calQty = q1 + q2 + 100L
            val order  = Algo.createOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
            val x      = process(order)
            x.head.isInstanceOf[UpdateOrder] shouldBe true
            val y = x.head.asInstanceOf[UpdateOrder]
            y.order.getQuantityD shouldBe (calQty - q1 - q2)
          }
        }
        "create a CancelOrder for o3" when {
          "calculated quantity is equal to o1 + o2 " in {
            val calQty = q1 + q2
            val order  = Algo.createOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
            val x      = process(order)
            x.size shouldBe 1
            x.head.isInstanceOf[CancelOrder] shouldBe true
            x.head.asInstanceOf[CancelOrder].order.getId shouldBe lastId
          }
        }
        "create two CancelOrder for o2 and o3" when {
          "calculated quantity is equal to o1" in {
            val calQty = q1
            val order  = Algo.createOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
            val x      = process(order)
            x.size shouldBe 2
            x.count(_.isInstanceOf[CancelOrder]) shouldBe 2
            x.head.asInstanceOf[CancelOrder].order.getId shouldBe lastId
            x.last.asInstanceOf[CancelOrder].order.getId shouldBe id2
          }
        }
        "create 2 CancelOrder for o2, o3 and 1 UpdateOrder for o1" when {
          "calculated quantity is less than q1" in {
            val calQty = q1 - 100
            val order  = Algo.createOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
            val x      = process(order)
            x.size shouldBe 3
            x.head.isInstanceOf[CancelOrder] shouldBe true
            x.head.asInstanceOf[CancelOrder].order.getId shouldBe lastId
            x(1).isInstanceOf[CancelOrder] shouldBe true
            x(1).asInstanceOf[CancelOrder].order.getId shouldBe id2
            x.last.isInstanceOf[UpdateOrder] shouldBe true
            val y = x.last.asInstanceOf[UpdateOrder]
            y.order.getQuantityL shouldBe (calQty - 0)
            y.order.getId shouldBe id1
          }
        }
        "create 1 CancelOrder for o3 and 1 UpdateOrder for o2" when {
          "calculated quantity is between o1 and o2" in {
            val calQty = q1 + 100
            val order  = Algo.createOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
            val x      = process(order)
            x.size shouldBe 2
            x.head.isInstanceOf[CancelOrder] shouldBe true
            x.head.asInstanceOf[CancelOrder].order.getId shouldBe lastId
            x.last.isInstanceOf[UpdateOrder] shouldBe true
            x.last.asInstanceOf[UpdateOrder].order.getId shouldBe id2
            val y = x.last.asInstanceOf[UpdateOrder]
            y.order.getQuantityL shouldBe (calQty - q1)
          }
        }
        "cancel all orders" when {
          "calculated quantity is 0" in {
            val calQty = 0
            val order  = Algo.createOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
            val x      = process(order)
            x.size shouldBe 3
            x.count(_.isInstanceOf[CancelOrder]) shouldBe 3
          }
        }
      }
      "live orders are empty and portfolio has enough position" should {
        "return InsertOrder" when {
          val process = baseProcess(List.empty, portfolioQty)
          "selling" in {
            val x = process(rawOrderSell)
            x.size shouldBe 1
            x.head.isInstanceOf[InsertOrder] shouldBe true
          }
        }
      }
      "live orders are empty and portfolio has nothing" should {
        val process = baseProcess(List.empty, 0L)
        "return InsertOrder" when {
          "buying" in {
            val x = process(rawOrderBuy)
            x.size shouldBe 1
            x.head.isInstanceOf[InsertOrder] shouldBe true
          }
        }
      }
      "live orders' direction clashes with that of order" should {
        val process = baseProcess(liveBuyOrders, portfolioQty)
        "cancel all orders" in {
          val x = process(rawOrderSell)
          x.count(_.isInstanceOf[CancelOrder]) shouldBe liveBuyOrders.size
        }
      }
      "live orders exist and portfolio doesn't have enough for new InsertOrder" when {
        "buying" should {
          "return an InsertOrder normally as buying doesn't depend on portfolio" in {
            val process = baseProcess(liveBuyOrders, 0L)
            val x       = process(rawOrderBuy)
            x.size shouldBe 1
            x.head.asInstanceOf[InsertOrder].order.getQuantityL shouldBe rawOrderBuy.getQuantityL - liveBuyOrders
              .map(_.getQuantityL)
              .sum
          }

        }
        "selling" should {
          "return an InsertOrder with quantity equal to what the portfolio has after deducted by live orders" in {
            val extra     = 100
            val portfolio = liveSellOrders.map(_.getQuantityL).sum + extra
            val process   = baseProcess(liveSellOrders, portfolio)
            val x         = process(rawOrderSell)
            x.size shouldBe 1
            x.head.asInstanceOf[InsertOrder].order.getQuantityL shouldBe extra
          }
        }
      }
      "live orders exist and portfolio has enough for new InsertOrder" when {
        "selling" should {
          "return an InsertOrder normally" in {
            val portfolio = portfolioQty
            val process   = baseProcess(liveSellOrders, portfolio)
            val x         = process(rawOrderSell)
            x.size shouldBe 1
            x.head.asInstanceOf[InsertOrder].order.getQuantityL shouldBe rawOrderSell.getQuantityL - liveBuyOrders
              .map(_.getQuantityL)
              .sum
          }
        }
      }
      "live orders don't exist when order with zero quantity arrives" should {
        "not send any order" in {
          val process = baseProcess(List.empty, 0L)
          val x       = process(Algo.createOrder(0, ulPrice, BuySell.BUY, CustomId.generate))
          x.size shouldBe 0
        }
      }
      "order does not have rounded quantity" should {
        "get the quantity rounded down to lot size" in {
          val process = baseProcess(List.empty, portfolioQty)
          val qty     = 6323
          val order   = createTestOrder("someid", 10L, qty, ulPrice, BuySell.SELL, CustomId.generate)
          val x       = process(order)
          x.head.asInstanceOf[InsertOrder].order.getQuantityL % lotSize shouldBe 0
        }
      }
    }
  }

  "handleOnSignal" should {
    "return Left" when {
      "pending orders exist" in {
        val x = for {
          a <- createApp[Id]().pure[Id]
          _ <- a.pendingOrdersRepo.put(InsertOrder(rawOrderBuy))
          c <- a.handleOnSignal(EitherT.fromEither(rawOrderBuy.asRight[Error]))
        } yield c
        x shouldBe Left(Error.PendingError)
      }
    }
    "return Right" when {
      "pending orders are empty" in {
        val x = for {
          a <- createApp[Id]().pure[Id]
          c <- a.handleOnSignal(EitherT.fromEither(rawOrderBuy.asRight[Error]))
        } yield c
        x shouldBe Right(())
      }
    }
  }

  "handleOnOrderAck" should {
    "return Left" when {
      "order is not available in the pending repo" in {
        val x = for {
          a <- createApp[Id]().pure[Id]
          c = a.handleOnOrderAck(CustomId.fromOrder(rawOrderBuy), EitherT.fromEither(rawOrderBuy.asRight[Error]))
        } yield c
        x.value shouldBe Left(
          Error.UnknownError(s"Pending order not found, custom id:${rawOrderBuy.getCustomField(CustomId.field)}")
        )
      }
    }
    "return Right" when {
      "order exists in the pending repo" in {
        val x = for {
          a <- createApp[Id]().pure[Id]
          _ <- a.pendingOrdersRepo.put(InsertOrder(rawOrderBuy))
          c = a.handleOnOrderAck(CustomId.fromOrder(rawOrderBuy), EitherT.fromEither(rawOrderBuy.asRight[Error]))
        } yield c
        x.value shouldBe Right(())
      }
    }
  }

  "getPriceAfterTicks" should {
    "return 32 when price is 30.75 ticked down 5 steps" in {
      Algo.getPriceAfterTicks(false, BigDecimal(32), 5) shouldBe 30.75
    }
    "return 24.70 when price is 25.50 ticked down 5 steps" in {
      Algo.getPriceAfterTicks(false, BigDecimal(25.50), 5) shouldBe 24.70
    }
    "return 101.5 when price is 99.5 ticked up 5 steps" in {
      Algo.getPriceAfterTicks(true, BigDecimal(99.50), 5) shouldBe 101.5
    }
    "return 98.25 when price is 99.5 ticked down 5 steps" in {
      Algo.getPriceAfterTicks(false, BigDecimal(99.50), 5) shouldBe 98.25
    }
  }
}
