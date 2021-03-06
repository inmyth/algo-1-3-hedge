package guardian

import com.ingalys.imc.order.Order
import horizontrader.services.collectors.persistent.ActiveOrderDescriptorView

import scala.util.Random

object Entities {

  case class Portfolio(symbol: String, position: Long)

  sealed trait SendingUrgency
  object SendingUrgency {
    case object Immediate extends SendingUrgency
    case object Later     extends SendingUrgency
  }

  sealed trait OrderAction
  object OrderAction {
    case class InsertOrder(order: Order, urgency: SendingUrgency)                              extends OrderAction
    case class UpdateOrder(activeOrderDescriptorView: ActiveOrderDescriptorView, order: Order) extends OrderAction
    case class CancelOrder(activeOrderDescriptorView: ActiveOrderDescriptorView, order: Order) extends OrderAction
  }

  sealed trait Direction
  object Direction {
    case object SELL extends Direction
    case object BUY  extends Direction
  }

  sealed trait PutCall
  object PutCall {
    case object PUT  extends PutCall
    case object CALL extends PutCall
  }

  case class CustomId(v: String)
  object CustomId {
    val field: Int                    = 14
    def generate: CustomId            = CustomId(Random.alphanumeric.take(15).mkString(""))
    def fromOrder(o: Order): CustomId = new CustomId(o.getCustomField(field).toString)
  }

  case class DwData(projVolume: Long, projPrice: Double, direction: Direction)

  case class RepoOrder(orderView: ActiveOrderDescriptorView, order: Order)
}
