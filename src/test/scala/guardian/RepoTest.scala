package guardian

import cats.Id
import cats.implicits._
import com.ingalys.imc.BuySell
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

  it should
    "append order, return all orders sorted by time down, remove order" in {
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

}
