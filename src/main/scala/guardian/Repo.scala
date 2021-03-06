package guardian

import cats.Applicative
import com.ingalys.imc.order.Order
import guardian.Entities.OrderAction.InsertOrder
import guardian.Entities.{CustomId, OrderAction, Portfolio, RepoOrder}
import horizontrader.services.collectors.persistent.ActiveOrderDescriptorView

import scala.language.higherKinds

abstract class LiveOrdersRepoAlgebra[F[_]] {

  def putOrder(symbol: String, order: RepoOrder): F[Unit]

  def getOrdersByTimeSortedDown(symbol: String): F[List[RepoOrder]]

  def getOrder(symbol: String, id: String): F[Option[RepoOrder]]

  def removeOrder(symbol: String, id: String): F[Unit]
}

class LiveOrdersInMemInterpreter[F[_]: Applicative] extends LiveOrdersRepoAlgebra[F] {
  private var db: Map[String, Map[String, RepoOrder]] = Map.empty

  override def putOrder(symbol: String, repoOrder: RepoOrder): F[Unit] = {
    val subMap = db.getOrElse(symbol, Map.empty)
    db += (symbol -> (subMap + (repoOrder.order.getId -> repoOrder)))
    Applicative[F].unit
  }

  override def getOrdersByTimeSortedDown(symbol: String): F[List[RepoOrder]] =
    Applicative[F].pure(
      db.getOrElse(symbol, Map.empty).values.toList.sortWith(_.order.getTimestampNanos > _.order.getTimestampNanos)
    )

  override def getOrder(symbol: String, id: String): F[Option[RepoOrder]] =
    Applicative[F].pure {
      db.getOrElse(symbol, Map.empty).get(id)
    }

  override def removeOrder(symbol: String, id: String): F[Unit] = {
    val subMap = db.getOrElse(symbol, Map.empty)
    db += (symbol -> (subMap - id))
    Applicative[F].unit
  }
}

abstract class UnderlyingPortfolioAlgebra[F[_]] {

  def put(symbol: String, portfolio: Portfolio): F[Unit]

  def get(symbol: String): F[Portfolio]

}

class UnderlyingPortfolioInterpreter[F[_]: Applicative] extends UnderlyingPortfolioAlgebra[F] {

  private var db: Map[String, Portfolio] = Map.empty

  override def put(symbol: String, portfolio: Portfolio): F[Unit] = {
    db += (symbol -> portfolio)
    Applicative[F].unit
  }

  override def get(symbol: String): F[Portfolio] =
    Applicative[F].pure(db.getOrElse(symbol, Portfolio(symbol, 0)))

}

abstract class PendingOrdersAlgebra[F[_]] {

  def putImmediate(act: OrderAction): F[Unit]

  def getImmediate(customId: CustomId): F[Option[OrderAction]]

  def removeImmediate(customId: CustomId): F[Unit]

  def isImmediateEmpty: F[Boolean]

  def putLater(act: InsertOrder): F[Unit]

  def isLaterEmpty: F[Boolean]

  def getLater: F[InsertOrder]

  def clearLater(): F[Unit]

}

class PendingOrdersInMemInterpreter[F[_]: Applicative] extends PendingOrdersAlgebra[F] {
  private var dbImmediate: Map[String, OrderAction] = Map.empty
  private var dbLater: Option[InsertOrder]          = None

  override def putImmediate(act: OrderAction): F[Unit] = {
    act match {
      case OrderAction.InsertOrder(order, _) => dbImmediate += (CustomId.fromOrder(order).v -> act)
      case OrderAction.UpdateOrder(_, order) => dbImmediate += (CustomId.fromOrder(order).v -> act)
      case OrderAction.CancelOrder(_, order) => dbImmediate += (CustomId.fromOrder(order).v -> act)
    }
    Applicative[F].unit
  }

  override def getImmediate(customId: CustomId): F[Option[OrderAction]] =
    Applicative[F].pure(dbImmediate.get(customId.v))

  override def removeImmediate(customId: CustomId): F[Unit] = {
    dbImmediate -= customId.v
    Applicative[F].unit
  }

  override def isImmediateEmpty: F[Boolean] = Applicative[F].pure(dbImmediate.isEmpty)

  override def putLater(act: InsertOrder): F[Unit] = {
    dbLater = Some(act)
    Applicative[F].unit
  }

  override def getLater: F[InsertOrder] = Applicative[F].pure(dbLater.get)

  override def clearLater(): F[Unit] = {
    dbLater = None
    Applicative[F].unit
  }

  override def isLaterEmpty: F[Boolean] = Applicative[F].pure(dbLater.isEmpty)
}

abstract class PendingCalculationAlgebra[F[_]] {

  def activate(): F[Unit]

  def deactivate(): F[Unit]

  def shouldRecalculate: F[Boolean]

}

class PendingCalculationInMemInterpreter[F[_]: Applicative] extends PendingCalculationAlgebra[F] {
  private var db: Boolean = false

  override def activate(): F[Unit] = {
    db = true
    Applicative[F].unit
  }

  override def deactivate(): F[Unit] = {
    db = false
    Applicative[F].unit
  }

  override def shouldRecalculate: F[Boolean] = Applicative[F].pure(db)
}
