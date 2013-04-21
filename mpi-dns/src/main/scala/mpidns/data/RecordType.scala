package mpidns.data

sealed abstract class RecordType(val id: Int)

case class A extends RecordType(1)
case class NS extends RecordType(2)
case class CNAME extends RecordType(5)
case class SOA extends RecordType(6)
case class PTR extends RecordType(12)
case class MX extends RecordType(15)
case class TXT extends RecordType(16)