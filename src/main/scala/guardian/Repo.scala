package guardian

import cats.Applicative
import com.ingalys.imc.order.Order
import guardian.Entities.{CustomId, OrderAction, Portfolio}

import scala.language.higherKinds

abstract class LiveOrdersRepoAlgebra[F[_]] {

  def putOrder(symbol: String, order: Order): F[Unit]

  def getOrdersByTimeSortedDown(symbol: String): F[List[Order]]

  def getOrder(symbol: String, id: String): F[Option[Order]]

  def removeOrder(symbol: String, id: String): F[Unit]
}

class LiveOrdersInMemInterpreter[F[_]: Applicative] extends LiveOrdersRepoAlgebra[F] {
  private var db: Map[String, Map[String, Order]] = Map.empty

  override def putOrder(symbol: String, order: Order): F[Unit] = {
    val subMap = db.getOrElse(symbol, Map.empty)
    db += (symbol -> (subMap + (order.getId -> order)))
    Applicative[F].unit
  }

  override def getOrdersByTimeSortedDown(symbol: String): F[List[Order]] =
    Applicative[F].pure(
      db.getOrElse(symbol, Map.empty).values.toList.sortWith(_.getTimestampNanos > _.getTimestampNanos)
    )

  override def getOrder(symbol: String, id: String): F[Option[Order]] =
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

  def put(act: OrderAction): F[Unit]

  def get(customId: CustomId): F[Option[OrderAction]]

  def remove(customId: CustomId): F[Unit]

  def isEmpty: F[Boolean]

}

class PendingOrdersInMemInterpreter[F[_]: Applicative] extends PendingOrdersAlgebra[F] {
  private var db: Map[String, OrderAction] = Map.empty

  override def put(act: OrderAction): F[Unit] = {
    act match {
      case OrderAction.InsertOrder(order) => db += (CustomId.fromOrder(order).v -> act)
      case OrderAction.UpdateOrder(order) => db += (CustomId.fromOrder(order).v -> act)
      case OrderAction.CancelOrder(order) => db += (CustomId.fromOrder(order).v -> act)
    }
    Applicative[F].unit
  }

  override def get(customId: CustomId): F[Option[OrderAction]] =
    Applicative[F].pure(db.get(customId.v))

  override def remove(customId: CustomId): F[Unit] = {
    db -= customId.v
    Applicative[F].unit
  }

  override def isEmpty: F[Boolean] = Applicative[F].pure(db.isEmpty)
}

abstract class PendingCalculationAlgebra[F[_]] {

  def put(derivativeSymbol: String): F[Unit]

  def remove(derivativeSymbol: String): F[Unit]

  def getAll: F[List[String]]

}

class PendingCalculationInMemInterpreter[F[_]: Applicative] extends PendingCalculationAlgebra[F] {
  private var db: Set[String] = Set.empty

  override def put(derivativeSymbol: String): F[Unit] = {
    db += derivativeSymbol
    Applicative[F].unit
  }

  override def remove(derivativeSymbol: String): F[Unit] = {
    db -= derivativeSymbol
    Applicative[F].unit
  }

  override def getAll: F[List[String]] = Applicative[F].pure(db.toList)

}
