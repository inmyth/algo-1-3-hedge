package guardian

import cats.Id
import cats.data.EitherT
import com.ingalys.imc.order.Order
import guardian.Algo.preProcess
import guardian.Entities.{Direction, OrderAction, PutCall}
import horizontrader.plugins.hmm.connections.service.IDictionaryProvider
import horizontrader.services.instruments.InstrumentDescriptor

case class Manager(algo: Algo[Id])

object Manager {

  def init(
      getDwList: (IDictionaryProvider, String, String) => List[InstrumentDescriptor],
      getDwProjectedPrice: InstrumentDescriptor => Option[Double],
      getOwnBestAskPrice: InstrumentDescriptor => Option[Double],
      calcUlQtyPreResidual: (Double, Double, Double, Double, String, String) => Long,
      getPutOrCall: InstrumentDescriptor => Option[PutCall],
      getUlProjectedPrice: (InstrumentDescriptor, Direction) => Either[Error, BigDecimal],
      getDelta: String => Option[Double],
      getPointValue: InstrumentDescriptor => Double,
      getAbsoluteResidual: (Double, String) => Option[BigDecimal],
      sendOrder: (OrderAction) => Order,
      logAlert: String => Unit,
      logInfo: String => Unit,
      logError: String => Unit,
      ulInstrument: InstrumentDescriptor,
      dictionaryService: IDictionaryProvider,
      hedgeInstrument: InstrumentDescriptor,
      context: String = "DEFAULT",
      exchangeName: String = "SET"
  ): Manager = {
    val preProcess: EitherT[Id, Error, Order] = Algo.preProcess[Id](
      getDwList,
      getDwProjectedPrice,
      getOwnBestAskPrice,
      calcUlQtyPreResidual,
      getPutOrCall,
      getUlProjectedPrice,
      getDelta,
      getPointValue,
      getAbsoluteResidual,
      ulInstrument,
      dictionaryService,
      hedgeInstrument,
      context,
      exchangeName
    )

    val algo = new Algo[Id](
      liveOrdersRepo = new LiveOrdersInMemInterpreter[Id](),
      portfolioRepo = new UnderlyingPortfolioInterpreter[Id](),
      pendingOrdersRepo = new PendingOrdersInMemInterpreter[Id](),
      pendingCalculationRepo = new PendingCalculationInMemInterpreter[Id](),
      underlyingSymbol = ulInstrument.getUniqueId,
      preProcess = preProcess,
      sendOrder = sendOrder,
      logAlert = logAlert,
      logInfo = logInfo,
      logError = logError
    )
    Manager(algo)
  }
}
