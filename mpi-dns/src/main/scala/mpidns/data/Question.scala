package mpidns.data

case class Question(
  qname: Name,
  qtype: Either[RecordType, Unit],
  qclass: Int)
