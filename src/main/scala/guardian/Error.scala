package guardian

sealed abstract class Error(val msg: String)
object Error {
  final case class UnknownError(m: String)    extends Error(m)
  final case class ValidationError(m: String) extends Error(m)
  final case class MarketError(m: String)     extends Error(m)
  final case class StateError(m: String)      extends Error(m)
  final case object PendingError              extends Error("Algo e. Cannot continue because there are pending orders")
}
