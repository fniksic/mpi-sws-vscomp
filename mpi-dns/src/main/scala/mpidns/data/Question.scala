package mpidns.data

case class Question(
  val qname: Name,
  val qtype: Either[RecordType, Unit],
  val qclass: Int) {
  override def toString() =
    "Question(" + qname.toString() + ", " + qtype.toString() + ", " + qclass.toString() + ")"
}
