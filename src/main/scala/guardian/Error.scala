package guardian

sealed abstract class Error(val msg: String)
object Error {
  final case class UnknownError(m: String) extends Error(m)
  final case class ValidationError(m: String) extends Error(m)
}

