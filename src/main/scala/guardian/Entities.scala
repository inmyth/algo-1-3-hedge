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


  case class DwData(projVolume: Long, projPrice: Double)

  //  def getMarketMakerBid(): F[Error Either BigDecimal]
  //
  //  def getMarketMakerAsk(): F[Error Either BigDecimal]

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
