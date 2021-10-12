package guardian

import cats.data.EitherT
import cats.implicits._
import cats.{Id, Monad}
import com.ingalys.imc.BuySell
import com.ingalys.imc.order.Order
import guardian.Entities.OrderAction.{CancelOrder, InsertOrder, UpdateOrder}
import guardian.Entities.{CustomId, OrderAction, Portfolio, RepoOrder}
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
        _ <-
          liveOrders
            .foreach(p => {
              a.liveOrdersRepo.putOrder(symbol, RepoOrder(createActiveOrderDescriptorView(p), p))
            })
            .pure[Id]
        c <- a.process(EitherT.fromEither(order.asRight[Error]))
      } yield c

  "process" when {
    "there are three orders on BUY side with quantity increasing along time" when {
      "calculated quantity is less than live orders quantity (trim: cancel or update)" when {
        val process = baseProcess(liveBuyOrders, portfolioQty)
        "calculated quantity is more than o1 + o2 but less than o1 + o2 + o3 and portfolio has enough position" should {
          "create an UpdateOrder for o3 " in {
            val calQty = q1 + q2 + 100L
            val order  = Algo.createPreProcessOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
            val x      = process(order)
            x.head.isInstanceOf[UpdateOrder] shouldBe true
            val y = x.head.asInstanceOf[UpdateOrder]
            y.order.getQuantityD shouldBe (calQty - q1 - q2)
          }
        }
        "calculated quantity is equal to o1 + o2" should {
          "create a CancelOrder for o3" in {
            val calQty = q1 + q2
            val order  = Algo.createPreProcessOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
            val x      = process(order)
            x.size shouldBe 1
            x.head.isInstanceOf[CancelOrder] shouldBe true
            x.head.asInstanceOf[CancelOrder].order.getId shouldBe lastId
          }
        }
        "calculated quantity is equal to o1" should {
          "create two CancelOrder for o2 and o3" in {
            val calQty = q1
            val order  = Algo.createPreProcessOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
            val x      = process(order)
            x.size shouldBe 2
            x.count(_.isInstanceOf[CancelOrder]) shouldBe 2
            x.head.asInstanceOf[CancelOrder].order.getId shouldBe lastId
            x.last.asInstanceOf[CancelOrder].order.getId shouldBe id2
          }
        }
        "calculated quantity is less than q1" should {
          "create 2 CancelOrder for o2, o3 and 1 UpdateOrder for o1" in {
            val calQty = q1 - 100
            val order  = Algo.createPreProcessOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
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
        "calculated quantity is between o1 and o2" should {
          "create 1 CancelOrder for o3 and 1 UpdateOrder for o2" in {
            val calQty = q1 + 100
            val order  = Algo.createPreProcessOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
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
        "calculated quantity is 0" should {
          "should cancel all orders" in {
            val calQty = 0
            val order  = Algo.createPreProcessOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
            val x      = process(order)
            x.size shouldBe 3
            x.count(_.isInstanceOf[CancelOrder]) shouldBe 3
          }
        }
        "if calculated quantity equals live orders' quantity" should {
          "do nothing" in {
            val calQty = q1 + q2 + q3
            val order  = Algo.createPreProcessOrder(calQty, ulPrice, BuySell.BUY, CustomId.generate)
            val x      = process(order)
            x.isEmpty shouldBe true
          }
        }
      }
    }
    "live orders' direction clashes with that of calculated order" should {
      val process = baseProcess(liveBuyOrders, portfolioQty)
      "cancel all orders" in {
        val x = process(rawOrderSell)
        x.count(_.isInstanceOf[CancelOrder]) shouldBe liveBuyOrders.size
      }
    }
    "calculated quantity is more than live orders quantity (should create one order)" when {
      "buy" when {
        "portfolio has zero" should {
          "return InsertOrder with calculated quantity (buying doesn't depend on portfolio)" in {
            val process = baseProcess(List.empty, 0L)
            val x       = process(rawOrderBuy)
            x.size shouldBe 1
            x.head.isInstanceOf[InsertOrder] shouldBe true
          }
          "return InsertOrder with quantity equals calculated quantity deducted by live orders' quantity" in {
            val process = baseProcess(liveBuyOrders, 0L)
            val x       = process(rawOrderBuy)
            x.size shouldBe 1
            x.head.asInstanceOf[InsertOrder].order.getQuantityL shouldBe rawOrderBuy.getQuantityL - liveBuyOrders
              .map(_.getQuantityL)
              .sum
          }
        }
      }
      "sell" when {
        "portfolio has enough position" when {
          "there are no live orders" should {
            "return InsertOrder with the original quantity" in {
              val process = baseProcess(List.empty, portfolioQty)
              val x       = process(rawOrderSell)
              x.size shouldBe 1
              x.head.isInstanceOf[InsertOrder] shouldBe true
            }
            "there are live orders" when {
              "portfolio doesn't have enough for desired quantity" should {
                "return an InsertOrder with quantity equal to what the portfolio has after deducted by live orders" in {
                  val extra     = 100
                  val portfolio = liveSellOrders.map(_.getQuantityL).sum + extra
                  val process   = baseProcess(liveSellOrders, portfolio)
                  val x         = process(rawOrderSell)
                  x.size shouldBe 1
                  x.head.asInstanceOf[InsertOrder].order.getQuantityL shouldBe extra
                }
              }
              "portfolio has enough for desired quantity" should {
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
          }
          "portfolio has zero" should {
            "should not send any order" in {
              val process = baseProcess(liveSellOrders, 0L)
              val x       = process(rawOrderSell)
              x.isEmpty shouldBe true
            }
          }
        }
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
    "calculated quantity is zero" should {
      "not send any order" in {
        val process = baseProcess(List.empty, 0L)
        val x       = process(Algo.createPreProcessOrder(0, ulPrice, BuySell.BUY, CustomId.generate))
        x.size shouldBe 0
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
    "return Right" when {
      "order exists in the pending repo" in {
        val x = for {
          a <- createApp[Id]().pure[Id]
          _ <- a.pendingOrdersRepo.put(InsertOrder(rawOrderBuy))
          c = a.handleOnOrderAck(
            createActiveOrderDescriptorView(rawOrderBuy),
            EitherT.fromEither(rawOrderBuy.asRight[Error])
          )
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
