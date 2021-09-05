package guardian

import com.ingalys.imc.order.Order

object Fixtures {

  def createOrder(id: String, nanos: Long, qty: Long, price: Double, buySell: Int): Order = {
    val x = Algo.createOrder(qty, price, buySell, id)
    x.setTimestampNanos(nanos) // assume we are the exchange who can assign timestamp
    x
  }
}
