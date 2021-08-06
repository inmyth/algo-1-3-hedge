package guardian

import com.ingalys.imc.order.Order

object Entities {


  case class Portfolio(symbol: String, position: Long)


  sealed trait OrderAction
  object OrderAction {
    case class InsertOrder(order: Order) extends OrderAction
    case class UpdateOrder(order: Order) extends OrderAction
    case class CancelOrder(id: String)   extends OrderAction
  }

}
