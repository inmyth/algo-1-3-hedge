package guardian

sealed trait Error
object Error {
  final case class UnexpectedError(msg: String) extends Error
  final case class ValidationError(msg: String) extends Error
}

