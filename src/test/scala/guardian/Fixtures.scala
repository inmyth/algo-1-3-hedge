package guardian

import com.ingalys.imc.BuySell
import com.ingalys.imc.order.Order
import guardian.Entities.CustomId

object Fixtures {
  val id1            = "id1"
  val id2            = "id2"
  val lastId         = "id3"
  val symbol: String = "PTT"
  val lotSize        = 100
  val ulPrice        = 50.0

  val customId1 = CustomId.generate
  val customId2 = CustomId.generate
  val customId3 = CustomId.generate

  val rawOrderBuy  = createTestOrder(id1, 10L, 500L, 50.0, BuySell.BUY, customId1)
  val rawOrderSell = createTestOrder(id2, 11L, 200L, 50.0, BuySell.SELL, customId2)

  def createTestOrder(hzId: String, nanos: Long, qty: Long, price: Double, buySell: Int, customId: CustomId): Order = {
    val o = new Order()
    o.setId(hzId)
    o.setQuantity(qty)
    o.setPrice(price)
    o.setBuySell(buySell)
    o.setTimestampNanos(nanos) // assume we are the exchange who can assign timestamp
    o.setCustomField(CustomId.field, customId.v)
    o
  }

  val liveOrders = List(
    createTestOrder(id1, 1L, 100L, 50.0, BuySell.BUY, customId1),
    createTestOrder(id2, 11L, 90L, 50.0, BuySell.BUY, customId2),
    createTestOrder(lastId, 111L, 30L, 50.0, BuySell.BUY, customId3)
  )

}
