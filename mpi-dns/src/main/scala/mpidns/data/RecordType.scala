package mpidns.data

sealed abstract class RecordType(val id: Int)

case object A extends RecordType(1)
case object NS extends RecordType(2)
case object CNAME extends RecordType(5)
case object SOA extends RecordType(6)
case object PTR extends RecordType(12)
case object MX extends RecordType(15)
case object TXT extends RecordType(16)