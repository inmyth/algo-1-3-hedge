package guardian

import com.ingalys.imc.BuySell
import com.ingalys.imc.order.Order
import guardian.Algo.{DW, MyScenarioStatus}
import guardian.Entities.CustomId
import guardian.Entities.PutCall.CALL
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

  val dw = DW(
    uniqueId = "RBF24C2201A@XBKK",
    projectedPrice = Some(0.25),
    projectedVol = Some(25100),
    delta = Some(0.06681047005390554),
    putCall = Some(CALL),
    marketSells = Vector(
      MyScenarioStatus(0.26, 1033000),
      MyScenarioStatus(0.27, 1091900),
      MyScenarioStatus(0.28, 1034400),
      MyScenarioStatus(0.29, 1094000),
      MyScenarioStatus(0.3, 1049800)
    ),
    marketBuys = Vector(
      MyScenarioStatus(0.25, 10000),
      MyScenarioStatus(0.24, 25100),
      MyScenarioStatus(0.23, 2046700),
      MyScenarioStatus(0.22, 1007700),
      MyScenarioStatus(0.21, 1055000)
    ),
    ownSellStatusesDefault = Vector(
      MyScenarioStatus(0.26, 1007900),
      MyScenarioStatus(0.27, 1091900),
      MyScenarioStatus(0.28, 1034400),
      MyScenarioStatus(0.29, 1094000),
      MyScenarioStatus(0.3, 1049800)
    ),
    ownBuyStatusesDefault = Vector(
      MyScenarioStatus(0.22, 1007700),
      MyScenarioStatus(0.21, 1055000),
      MyScenarioStatus(0.2, 1007700),
      MyScenarioStatus(0.19, 1096600),
      MyScenarioStatus(0.23, 1046700)
    ),
    ownSellStatusesDynamic = Vector(MyScenarioStatus(0.26, 25100)),
    ownBuyStatusesDynamic = Vector(MyScenarioStatus(0.24, 25100))
  )

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
