package guardian

import com.ingalys.imc.BuySell
import com.ingalys.imc.order.Order
import guardian.Entities.CustomId
import horizontrader.services.collectors.persistent.ActiveOrderDescriptorView
import horizontrader.services.instruments.InstrumentDescriptor

object Fixtures {

  val symbol: String = "PTT"
  val id1            = "id1"
  val id2            = "id2"
  val lastId         = "id3"
  val lotSize        = 100
  val ulPrice        = 50.0
  val portfolioQty   = 8000L
  val q1             = 3600L
  val q2             = 1100L
  val q3             = 300L

  val customBuyId1  = CustomId.generate
  val customBuyId2  = CustomId.generate
  val customBuyId3  = CustomId.generate
  val customSellId1 = CustomId.generate
  val customSellId2 = CustomId.generate
  val customSellId3 = CustomId.generate

  val liveBuyOrders = List(
    createTestOrder(id1, 10L, q1, ulPrice, BuySell.BUY, customBuyId1),
    createTestOrder(id2, 11L, q2, ulPrice, BuySell.BUY, customBuyId2),
    createTestOrder(lastId, 12L, q3, ulPrice, BuySell.BUY, customBuyId3)
  )

  val liveSellOrders = List(
    createTestOrder(id1, 10L, q1, ulPrice, BuySell.SELL, customSellId1),
    createTestOrder(id2, 11L, q2, ulPrice, BuySell.SELL, customSellId2),
    createTestOrder(lastId, 12L, q3, ulPrice, BuySell.SELL, customSellId3)
  )

  val rawOrderBuy  = createTestOrder(id1, 10L, 7200L, 50.0, BuySell.BUY, customBuyId1)
  val rawOrderSell = createTestOrder(id2, 11L, 6500L, 50.0, BuySell.SELL, customSellId1)

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

  def createActiveOrderDescriptorView(order: Order): ActiveOrderDescriptorView =
    new ActiveOrderDescriptorView(InstrumentDescriptor.newInstrumentDescriptor(null, null, symbol, symbol), order)
}
