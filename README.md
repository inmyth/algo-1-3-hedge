# Algo-1-3 Hedge V2

Lot size = ulInstrument.getLotSizeD


Read me here
Controller flow: (to be deleted)
```
import algotrader.api.Messages.{Load, Start}
import algotrader.api.NativeController
import com.hsoft.datamaster.product.{Derivative, ProductTypes}
import com.hsoft.hmm.api.automaton.spi.DefaultAutomaton
import com.hsoft.hmm.api.source.position.RiskPositionDetailsSourceBuilder
import com.hsoft.hmm.api.source.pricing.{Pricing, PricingSourceBuilder}
import com.hsoft.hmm.posman.api.position.container.RiskPositionDetailsContainer
import com.ingalys.imc.BuySell
import com.ingalys.imc.depth.DepthOrder
import com.ingalys.imc.summary.Summary
import horizontrader.plugins.hmm.connections.service.IDictionaryProvider
import horizontrader.services.instruments.{InstrumentDescriptor, InstrumentInfoService}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.language.higherKinds

// case class MyMessage(myParam : Int)

trait Controller extends NativeController {
  val portfolioId:String
  val ulId:String = "PTT"
  val ulInstrument: InstrumentDescriptor
  val hedgeInstrument:InstrumentDescriptor
  val dictionaryService: IDictionaryProvider = getService[IDictionaryProvider]
  import algotrader.api.source.summary._

  def getDwList(ds: IDictionaryProvider, ulId: String, exchangeName: String = "SET"): List[InstrumentDescriptor] =
    ds.getDictionary.getProducts(null).values().asScala
      .map(p => p.asInstanceOf[Derivative])
      .filter(d => d.getUlId == ulId && d.getProductType == ProductTypes.WARRANT)
      .map(p => getService[InstrumentInfoService].getInstrumentByUniqueId(exchangeName, p.getId) )
      .toList

  def getProjectedPrice(inDe: InstrumentDescriptor): Option[Double] = source[Summary].get(inDe).latest.flatMap(_.theoOpenPrice)
  def getProjectedVolume(inDe: InstrumentDescriptor): Option[Long] = source[Summary].get(inDe).latest. flatMap(_.theoOpenVolume)

  def getOwnMatchedOrder(projectedPrice: Double, projectedVolume: Double): Int = ???
  def getDelta(dwId: String): Option[Double] = source[Pricing].get(dwId,"DEFAULT").latest.map(_.delta) // negative = put

  def calcResidual(dwList: List[InstrumentDescriptor]) = // negative = offer/sell order
    (dwList.map(p => getDelta(p.getUniqueId)), dwList.map(getProjectedVolume))
      .zipped
      .toList
      .map {
        case (Some(delta), Some(vol)) => vol * Math.abs(delta) // if delta can be negative then make it absolute
        case _ => 0
      }
      .sum

  def calcTotalResidual(dwList: List[InstrumentDescriptor], ul: InstrumentDescriptor) =
    calcResidual(dwList) + calcResidual(List(ul))


  def main: Unit = {
    val dwList = getDwList(dictionaryService, ulId)

    val dwProjectedPriceList = dwList.map(getProjectedPrice)
    val dwProjectedVolumeList = dwList.map(getProjectedVolume)
    val deltaList = dwList.map(p => getDelta(p.getUniqueId))
    val totalResidual = calcTotalResidual(dwList, ulInstrument)
    val ownMatchedOrderList = (dwProjectedPriceList, dwProjectedVolumeList, deltaList)
      .zipped
      .toList
      .map{
        case (Some(price), Some(vol), Some(delta)) => getOwnMatchedOrder(price, vol) * Math.abs(delta)
        case _ => 0
      }
    val preMainOrder = ownMatchedOrderList.sum + totalResidual
    
    // send own order to main calculation
  }

  onMessage {

    case Load =>
      /*
        Get all derivative warrants
        - What is "SET" ? exchange name
       */
      val dwList: List[InstrumentDescriptor] = dictionaryService.getDictionary.getProducts(null).values().asScala
        .map(p => p.asInstanceOf[Derivative])
        .filter(d => d.getUlId == ulId && d.getProductType == ProductTypes.WARRANT)
        .map(p => getService[InstrumentInfoService].getInstrumentByUniqueId("SET", p.getId) )
        .toList


      /*
        Get a particular dw
       */

      val dwId = "PTT24CA"
      val dwPTT24CA = dwList.find(p => p.getUniqueId == dwId)


      /*
        Get projected price, projected volume of a dw
       */
      val ssDwPTT24CA = source[Summary].get(dwPTT24CA.get) // .map(_.modeStr.get)
      val ssDwPTT24CA_ProjectedPrice =  ssDwPTT24CA.latest.get.theoOpenPrice
      val ssDwPTT24CA_ProjectedVolume = ssDwPTT24CA.latest.get.theoOpenVolume

      /*
      Get own quantity
      Confirm with others
     */

      val x= DepthOrder
      val automan = new DefaultAutomaton("SET-EMAPI-HMM-PROXY", dwId, "DEFAULT")
      log.info (s"ProductID="+automan.getProductId)
//      val ownBidPrice = automan.getBidPrice
      val ownBidOrderQty = automan.getBaseBidSize
      val ownAskOrderQty = automan.getBaseAskSize


      /*
      Get someone else's volume
        if we know otherPeopleVol we will find ourOwnMatchedVol

        if()


        else

       */

      /*
  Get delta
  - Is the symbol (dwId) correct ?
 */
      val ssDwPTT24CA_pricingSource = source[Pricing].get(dwId,"DEFAULT")
      val delta = ssDwPTT24CA_pricingSource.latest.get.delta

    /*

      Get residual
      Nop: Sum(ProjDWMatchQty*delta)
      PTT
      PTT24C2201A CallDW Delta 0.01  Sell ProjDWMatchQty  20,000  ProjDWMatchQty*delta = Buy UL 200
      PTT24C2201B CallDW Delta 0.02  Buy ProjDWMatchQty  20,000  ProjDWMatchQty*delta = Sell UL -400
      PTT24P2201A PutDW Delta -0.01 Sell ProjDWMatchQty  10,000  ProjDWMatchQty*delta = Sell UL -100
      PTT24P2201B PutDW Delta -0.04 Buy ProjDWMatchQty  10,000  ProjDWMatchQty*delta = Buy UL 400

      residual sumDW = (200+-400+-100+400) = 100 Buy UL 100

      residual portfolio can get from portfolio Buy UL 300

      residual sumDW + residual portfolio Send the order

     */



      /*
        Revisit:
        Get direction (ask / bid)

          Method 1:
          Projected price <= automation best bid : CALL Sell UL
          Projected price >= automation best ask : CALL Buy UL
          Projected price <= automation best bid : PUTDW Buy UL
          Projected price >= automation best ask : PUTDW Sell UL

          Method2:
          Look at the name of the dw market

       */





    /*
       At this point we have the quantity of the order we need to send to underlying market
       - DwData :
          dw projected price,
          dw projected volume,
          ownVolume,
          hedge ratio,
          residual,
          call or put
     */


      /*
        Get underlying projected price
       */
      val ssPTT = source[Summary].get(ulInstrument)
      val ssPTT_ProjectedPrice =  ssPTT.latest.get.theoOpenPrice.get


      /*
        Get the new ask/bid price counted 5 ticks from projected price
        - Check the behavior
          it should "return 32 when price is 30.75 ticked down 5 steps"
          it should "return 24.70 when price is 25.50 ticked down 5 steps"
          it should "return 101.5 when price is 99.5 ticked up 5 steps"
          it should "return 98.25 when price is 99.5 ticked down 5 steps"
       */
      val sellingPrice = getPriceAfterTicks(5, ssPTT_ProjectedPrice)
      def getPriceAfterTicks(ticks: Int, price: Double): Double = ???

//      def SETChkSpread(refPrice:Double): Double ={
//        var chkSpread:Double = 0
//        if (refPrice>=0 && refPrice < 2) {
//          chkSpread = 0.01
//        } else if (refPrice>=2 && refPrice<5){
//          chkSpread = 0.02
//        } else if (refPrice>=5 && refPrice<10){
//          chkSpread = 0.05
//        } else if (refPrice>=10 && refPrice<25){
//          chkSpread = 0.1
//        } else if (refPrice>=25 && refPrice<100){
//          chkSpread = 0.25
//        } else if (refPrice>=100 && refPrice<200){
//          chkSpread = 0.5
//        } else if (refPrice>=200 && refPrice<400){
//          chkSpread = 1
//        } else if (refPrice>=400){
//          chkSpread = 2
//        } else {
//          chkSpread = 0
//        }
//         chkSpread
//      }


    /*
      Check portfolio's position
      - Check ulId
     */

      val pttPortfolio = source[RiskPositionDetailsContainer].get(portfolioId,ulId, true)
      val pttPosition = pttPortfolio.latest.get.getTotalPosition.getNetQty

      /*
         Main calculation happens here
        Use The price and quantity to create orders to send to underlying market
       */

     /*
         Send Limit order(s)
         - Check instrument
         - portfolioId
     */
//      sendLimitOrder(instrument, BuySell.BUY, qty, roundPrice(instrument,price), (o, i) => {
//        o.setCustomField(ReferenceMarketDataField.PORTFOLIO, portfolioId)
//        o
//      })
    case Start =>
  }

//  def createApp[F[_]: Monad](symbol: String): Algo[F] = {
//    val a = new LiveOrdersInMemInterpreter[F]
//    val b = new UnderlyingPortfolioInterpreter[F]
//    val c = new PendingOrdersInMemInterpreter[F]
//    val d = new PendingCalculationInMemInterpreter[F]
//    val e = Map.empty[String, Source[Summary]]
//    val f = (s: String, d: Double) => ()
//    val g = (s: String) => (log.info(s))
//    new Algo[F](a, b, c, d, symbol,  e, f, g)
//  }
//
//  def createOrder(id: String, nanos: Long, qty: Long, price: Double, buySell: Int): Order = {
//    val x = Algo.createOrder(qty, price, buySell, id)
//    x.setTimestampNanos(nanos) // assume we are the exchange who can assign timestamp
//    x
//  }
//  val rawOrderBuy = createOrder("id1", 10L, 500L, 50.0, BuySell.BUY)
//  val dw1 = "PTT@ABC"
//val symbol ="PTT"
//  onMessage {
//    // react on message here
//    // case MyMessage(i) =>
//    case Load =>
//      val x = for {
//        a <- Monad[Id].pure(createApp[Id](symbol))
//        _ <- a.pendingOrdersRepo.put(InsertOrder(rawOrderBuy))
//        c <- a.handleOnSignal(dw1)
//      } yield (c)
//
//      log.info(s"AAA  $x")
//      // Algo.init(dw, ul)
//
//      val calQty = 135L
//      val q1     = 100L
//      val q2     = 30L
//      val q3     = 10L
//      val liveOrders = List(
//        createOrder("id1", 10L, q1, 50.0, BuySell.BUY),
//        createOrder("id2", 11L, q2, 50.0, BuySell.BUY),
//        createOrder("id3", 12L, q3, 50.0, BuySell.BUY)
//      )
//      val y = for {
//        a <- Monad[Id].pure(createApp[Id](symbol))
//        _ = liveOrders.foreach(a.liveOrdersRepo.putOrder(symbol, _))
//        b = a.liveOrdersRepo.getOrdersByTimeSortedDown(symbol)
//        c = a.trimLiveOrders(b, calQty, ListBuffer.empty)
//      } yield c
//      log.info(s"BBB  $y")
//        // instrument.onUpdate(algo.onUpdate)
//
//    case Start =>
//
//  }
//


}


/*
package simple1

import algotrader.api.NativeController
import algotrader.api.Messages._
import com.hsoft.hmm.api.automaton.spi.DefaultAutomaton
import com.hsoft.hmm.api.automaton.{Automaton, AutomatonReaderFactory}
import com.hsoft.hmm.api.source.position.RiskPositionDetailsSourceBuilder
import com.hsoft.hmm.api.source.pricing.{Pricing, PricingSourceBuilder}
import com.hsoft.hmm.posman.api.position.container.RiskPositionDetailsContainer
import com.ingalys.imc.order.Validity
import horizontrader.plugins.hmm.connections.service.StoreSelectionFactoryProvider
import horizontrader.plugins.hmm.services.automaton.AutoManagerService

import java.util.Objects.isNull


//dictionary
import horizontrader.plugins.hmm.connections.service.IDictionaryProvider
//instrument
import horizontrader.services.instruments.InstrumentDescriptor
//summarySource
import algotrader.api.source.summary.SummarySourceBuilder
import com.ingalys.imc.summary._
//summaryDepth
import algotrader.api.source.depth.DepthSourceBuilder
import com.ingalys.imc.depth._
//Get DW under UL
import scala.collection.JavaConverters._
import com.hsoft.datamaster.product.ProductTypes
import horizontrader.services.instruments.InstrumentInfoService
import com.hsoft.datamaster.product.Derivative
//Get DeltaCash
import com.hsoft.hmm.posman.api.position.container.RiskPositionByULContainer
import com.hsoft.hmm.api.source.position.RiskPositionByUlSourceBuilder
//Get Delta
import com.hsoft.hmm.api.source.automatonstatus.AutomatonStatus
import com.ingalys.imc.uss.dictgen.dict.USSDictGenDictManager.Source

trait controller extends NativeController {
  val instrument: InstrumentDescriptor
  val summarySource = source[Summary].get(instrument)
  val summaryDepth = source[Depth].get(instrument)
  val dictionaryService = getService[IDictionaryProvider]
  val ulId:String
  val portfolioId:String = "JV"

  val SPOT_BID_CF = 0x40001001
  val SPOT_ASK_CF = 0x40001002
  val DELTA_CF = 0x40000123
  val GAMMA_CF = 0x40001000

  onMessage {
    // react on message here
    // case MyMessage(i) =>
    case Load =>{
      //summarySource.filter(s => s.sellPrice.isDefined).map(s=> TickerInit(s.sellPrice.get)).onUpdate(in => processMessage(in))
      summarySource.filter(s=>s.theoOpenPrice.isDefined).onUpdate(s => log.info(s"ProjPx= " + s.theoOpenPrice.get))
      summarySource.filter(s=>s.theoOpenVolume.isDefined).onUpdate(s => log.info(s"ProjQty= " + s.theoOpenVolume.get))
      summarySource.filter(s=>s.buyPrice.isDefined).onUpdate(s => log.info(s"BestBid= " + s.buyPrice.get)) //BestBidPx
      summarySource.filter(s=>s.buyQty.isDefined).onUpdate(s => log.info(s"QtyBid= " + s.buyQty.get))
      summarySource.filter(s=>s.sellPrice.isDefined).onUpdate(s => log.info(s"BestOffer= " + s.sellPrice.get)) //BestAskPx
      summarySource.filter(s=>s.sellQty.isDefined).onUpdate(s => log.info(s"QtyOffer= " + s.sellQty.get))
      summarySource.filter(s=>s.last.isDefined).onUpdate(s => log.info(s"LastPx= " + s.last.get)) //LastPx
      summarySource.filter(s=>s.closePrevious.isDefined).onUpdate(s => log.info(s"PrevPx= " + s.closePrevious.get)) //Previous Close Price

      //summaryDepth.onUpdate(d => log.info(s"Bid1= "+d.getBuy(1)))
      summaryDepth.filter(d => d.getBuy(0) != null).onUpdate(d => log.info(s"Bid0= "+d.getBuy(0).getPrice + "BidQty0="+d.getBuy(0).getQuantityL))
      //summaryDepth.filter(d => d.getSell(0) != null).onUpdate(d => log.info(s"Ask0= "+d.getSell(0).getPrice + "AskQty0="+d.getSell(0).getQuantityL))

      summaryDepth.onUpdate(d => {
        log.info(s"BuyCount="+d.getBuyCount)
        var i:Int = 0
        for (i <- 0 until d.getBuyCount) {
          log.info(s"MktBid"+i+"="+d.getBuy(i).getPrice+" MktBidQty"+i+"="+d.getBuy(i).getQuantityL)
        }
        log.info(s"SellCount="+d.getSellCount)
        i=0
        for (i <- 0 until d.getSellCount) {
          log.info(s"MktAsk"+i+"="+d.getSell(i).getPrice+" MktAskQty"+i+"="+d.getBuy(i).getQuantityL)
        }
      })

      source[RiskPositionByULContainer].get(portfolioId, ulId, true).onUpdate(p => {
        log.info (s"Total Delta Cash="+p.getTotalPosition.getDeltaCashUlCurr)
        log.info (s"Today Delta Cash="+p.getTodayPosition.getDeltaCashUlCurr)
        log.info (s"Yesterday Delta Cash="+p.getYesterdayPosition.getDeltaCashUlCurr)
      })

      //val automatonSource = source[AutomatonStatus].get("XBKK","DEFAULT","BTS19C2110A@XBKK","REFERENCE").onUpdate(a => {})

      val automan = new DefaultAutomaton("SET-EMAPI-HMM-PROXY","BTS19C2110A@XBKK","DEFAULT")
      log.info (s"ProductID="+automan.getProductId)
      log.info (s"BaseBidSize="+automan.getBaseBidSize)
      log.info (s"BaseAskSize="+automan.getBaseAskSize)

      val SymbolID:String = "BTS01C2109A@XBKK"

      val PricingSource = source[Pricing].get(SymbolID,"DEFAULT")
      PricingSource.filter(p => p.getDelta != null).onUpdate(p => log.info(s"ProductID="+p.getProductId+"Delta="+p.getDelta))

      val PositionSource = source[RiskPositionDetailsContainer].get(portfolioId,"BTS19C2110A@XBKK", true)
      PositionSource.onUpdate(p => log.info(s"TotalPosition of "+p.getProductId+" TotalPosition="+p.getTotalPosition.getNetQty))

      dictionaryService.getDictionary.getProducts(null).values().asScala
        .filter(p => p.getProductType == ProductTypes.WARRANT)
        .map(p => p.asInstanceOf[Derivative])
        .filter(d => d.getUlId == ulId)
        .foreach(d => {
            var DWinstrument:InstrumentDescriptor = null
            if (d.getProductType == ProductTypes.WARRANT) {
              DWinstrument = getService[InstrumentInfoService].getInstrumentByUniqueId("SET", d.getId)
              //log.info(s"DWinstrument="+DWinstrument)
              log.info(s"Subscribing to Executions on [${d}]")
            }
        })
    }
    case Start =>
  }

}
 */


```