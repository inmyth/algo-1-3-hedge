package guardian

import cats.Id
import cats.implicits._
import com.ingalys.imc.BuySell
import guardian.Entities.Portfolio
import org.scalatest.flatspec.AnyFlatSpec
import guardian.Shared.createOrder
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class RepoTest extends AnyFlatSpec{

  behavior of "LiveOrdersInMem"


  val liveRepo = new LiveOrdersInMemInterpreter[Id]

  val id1 = "id1"
  val liveOrders = List(
    createOrder(id1, 1L,  100L, 50.0, BuySell.BUY),
    createOrder("id2", 11L, 90L, 50.0, BuySell.BUY),
    createOrder("id3", 111L, 30L, 50.0, BuySell.BUY)
  )

  val symbol: String = "PTT"

  it should "append order, return all orders sorted by time down, remove order" in {
    liveOrders.foreach(liveRepo.putOrder(symbol, _))
    val a = liveRepo.getOrder(symbol, id1)
    a.get.getId shouldBe id1
    val b = liveRepo.getOrdersByTimeSortedDown(symbol)
    b.size shouldBe liveOrders.size

    liveRepo.removeOrder(symbol, id1)
    val c = liveRepo.getOrder(symbol, id1)
    c shouldBe None

    val d = liveRepo.getOrdersByTimeSortedDown(symbol)
    d.size shouldBe liveOrders.size - 1
  }

  behavior of "UnderlyingPortfolioInterpreter"
  val ulRepo = new UnderlyingPortfolioInterpreter[Id]()

  it should "put, get portfolio" in {
    val position = 55500L
    val p = Portfolio(symbol, position )

    ulRepo.put(symbol, p)
    val b = ulRepo.get(symbol)
    b.position shouldBe position
  }

  behavior of "PendingOrdersInMemInterpreter"
  val pendingOrdersRepo = new PendingOrdersInMemInterpreter[Id]()

  it should "put, get, remove order" in {
    pendingOrdersRepo.put(liveOrders.head)
    pendingOrdersRepo.put(liveOrders.head)

    val a = pendingOrdersRepo.get(id1)
    a.isDefined shouldBe true
    a.get.getId shouldBe id1

    pendingOrdersRepo.remove(id1)
    val b = pendingOrdersRepo.get(id1)
    b shouldBe None
  }

  behavior of "PendingOrdersInMemInterpreter"
  val pendingCalculationRepo = new PendingCalculationInMemInterpreter[Id]()

  it should "put, remove, return shouldCalculate" in {
    pendingCalculationRepo.put(symbol)
    val a = pendingCalculationRepo.shouldCalculate(symbol)
    a shouldBe true

    pendingCalculationRepo.remove(symbol)
    val b = pendingCalculationRepo.shouldCalculate(symbol)
    b shouldBe false
  }
}
