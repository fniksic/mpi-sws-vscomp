package mpidns.data

case class Header(
  val id: Int,
  val response: Boolean,
  val opCode: Int,
  val authoritative: Boolean,
  val truncated: Boolean,
  val recursionDesired: Boolean,
  val recursionAvailable: Boolean,
  val zero: Int = 0,
  val rCode: Int,
  val questionCount: Int,
  val answerCount: Int,
  val authorityCount: Int,
  val additionalCount: Int)