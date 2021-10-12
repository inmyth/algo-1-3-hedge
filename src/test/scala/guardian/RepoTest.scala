package guardian

import cats.Id
import com.ingalys.imc.BuySell
import guardian.Entities.OrderAction.{CancelOrder, InsertOrder, UpdateOrder}
import guardian.Entities.{Portfolio, RepoOrder}
import guardian.Fixtures._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class RepoTest extends AnyFlatSpec {

  behavior of "LiveOrdersInMem"

  val liveRepo = new LiveOrdersInMemInterpreter[Id]

  it should "append order, return all orders sorted by time down, remove order" in {
    liveBuyOrders.foreach(p => liveRepo.putOrder(symbol, RepoOrder(createActiveOrderDescriptorView(p), p)))
    val a = liveRepo.getOrder(symbol, id1)
    a.get.orderView.getOrderCopy.getId shouldBe id1
    val b = liveRepo.getOrdersByTimeSortedDown(symbol)
    b.size shouldBe liveBuyOrders.size

    liveRepo.removeOrder(symbol, id1)
    val c = liveRepo.getOrder(symbol, id1)
    c shouldBe None

    val d = liveRepo.getOrdersByTimeSortedDown(symbol)
    d.size shouldBe liveBuyOrders.size - 1
  }

  behavior of "UnderlyingPortfolioInterpreter"
  val ulRepo = new UnderlyingPortfolioInterpreter[Id]()

  it should "put, get portfolio" in {
    val position = 55500L
    val p        = Portfolio(symbol, position)

    ulRepo.put(symbol, p)
    val b = ulRepo.get(symbol)
    b.position shouldBe position
  }

  behavior of "PendingOrdersInMemInterpreter"
  val pendingOrdersRepo = new PendingOrdersInMemInterpreter[Id]()

  it should "put, get, remove InsertOrder" in {
    val a = InsertOrder(liveBuyOrders.head)
    pendingOrdersRepo.put(a)
    pendingOrdersRepo.put(a)

    val b = pendingOrdersRepo.get(customBuyId1)
    b.isDefined shouldBe true
    b.get.isInstanceOf[InsertOrder] shouldBe true
    b.get.asInstanceOf[InsertOrder].order.getId shouldBe id1

    pendingOrdersRepo.remove(customBuyId1)
    val c = pendingOrdersRepo.get(customBuyId1)
    c shouldBe None
  }

  it should "put, get, remove UpdateOrder" in {
    val a = UpdateOrder(liveBuyOrders.head)
    pendingOrdersRepo.put(a)

    val b = pendingOrdersRepo.get(customBuyId1)
    b.isDefined shouldBe true
    b.get.isInstanceOf[UpdateOrder] shouldBe true
    b.get.asInstanceOf[UpdateOrder].order.getId shouldBe id1

    pendingOrdersRepo.remove(customBuyId1)
    val c = pendingOrdersRepo.get(customBuyId1)
    c shouldBe None
  }

  it should "put, get, remove CancelOrder" in {
    val a = CancelOrder(liveBuyOrders.head)
    pendingOrdersRepo.put(a)

    val b = pendingOrdersRepo.get(customBuyId1)
    b.isDefined shouldBe true
    b.get.isInstanceOf[CancelOrder] shouldBe true
    b.get.asInstanceOf[CancelOrder].order.getId shouldBe id1

    pendingOrdersRepo.remove(customBuyId1)
    val c = pendingOrdersRepo.get(customBuyId1)
    c shouldBe None
  }

  behavior of "PendingCalculationInMemInterpreter"
  val pendingCalculationRepo = new PendingCalculationInMemInterpreter[Id]()

  it should "put, remove, getAll" in {
    pendingCalculationRepo.activate()
    val a = pendingCalculationRepo.shouldRecalculate
    a shouldBe true

    pendingCalculationRepo.deactivate()
    val b = pendingCalculationRepo.shouldRecalculate
    b shouldBe false
  }
}
