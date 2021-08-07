package guardian

import cats.{Applicative, Monad}
import com.ingalys.imc.order.Order
import guardian.Entities.Portfolio
import guardian.Error.UnexpectedError

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
    Applicative[F].pure(db.getOrElse(symbol, Map.empty).values.toList.sortWith(_.getTimestampNanos > _.getTimestampNanos))

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

class UnderlyingPortfolioInterpreter[F[_]: Monad] extends UnderlyingPortfolioAlgebra[F] {

  private var db: Map[String, Portfolio] = Map.empty

  override def put(symbol: String, portfolio: Portfolio): F[Unit] = {
    db += (symbol -> portfolio)
    Monad[F].unit
  }

  override def get(symbol: String): F[Portfolio] =
    Monad[F].pure(db.getOrElse(symbol, Portfolio(symbol, 0)))

}

abstract class PendingOrdersAlgebra[F[_]] {

  def put(order: Order): F[Unit]

  def get(id: String): F[Option[Order]]

  def remove(id: String): F[Unit]

}

class PendingOrdersInMemInterpreter[F[_]: Monad] extends PendingOrdersAlgebra[F] {
  private var db: Map[String, Order] = Map.empty

  override def put(order: Order): F[Unit] = {
    db += (order.getId -> order)
    Monad[F].unit
  }

  override def get(id: String): F[Option[Order]] =
    Monad[F].pure(db.get(id))

  override def remove(id: String): F[Unit] = {
    db -= id
    Monad[F].unit
  }
}

abstract class PendingCalculationAlgebra[F[_]] {

  def put(derivativeSymbol: String): F[Unit]

  def remove(derivativeSymbol: String): F[Unit]

  def shouldCalculate(derivativeSymbol: String): F[Boolean]

}

class PendingCalculationInMemInterpreter[F[_]: Monad] extends PendingCalculationAlgebra[F] {
  private var db: Set[String] = Set.empty

  override def put(derivativeSymbol: String): F[Unit] = {
    db += derivativeSymbol
    Monad[F].unit
  }

  override def shouldCalculate(derivativeSymbol: String): F[Boolean] = Monad[F].pure(db(derivativeSymbol))

  override def remove(derivativeSymbol: String): F[Unit] = {
    db -= derivativeSymbol
    Monad[F].unit
  }
}