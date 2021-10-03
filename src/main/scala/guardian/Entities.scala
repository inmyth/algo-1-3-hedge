package guardian

import com.ingalys.imc.order.Order

import java.util.UUID

object Entities {

  case class Portfolio(symbol: String, position: Long)

  sealed trait OrderAction
  object OrderAction {
    case class InsertOrder(order: Order) extends OrderAction
    case class UpdateOrder(order: Order) extends OrderAction
    case class CancelOrder(order: Order) extends OrderAction
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
    def generate: CustomId            = CustomId(UUID.randomUUID().toString)
    def fromOrder(o: Order): CustomId = new CustomId(o.getCustomField(field).toString)
  }

  case class DwData(projVolume: Long, projPrice: Double, direction: Direction)

  //  def getMarketMakerBid(): F[Error Either BigDecimal]

  //  def getMarketMakerAsk(): F[Error Either BigDecimal] // Automaton = Market maker

//  def getProjectedPrice(): F[Error Either BigDecimal] // this can come from Horizon
//
//  def getProjectedVolume(): F[Error Either Int] // comes from Horizon but needs to be adjusted
//
//  def getHedgeRatio(): F[Error Either BigDecimal]
//
//  def getResidual() = ???
//
//  def getLastUnderlyingProjectedPrice(): F[Error Either BigDecimal]

}
