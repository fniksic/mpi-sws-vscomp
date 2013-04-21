package mpidns.data

case class Question(
  val qname: Name,
  val qtype: Either[RecordType, Unit],
  val qclass: Int)
