package mpidns.data

class Question(
  qname: Name,
  qtype: Either[RecordType, Unit],
  qclass: Int)
