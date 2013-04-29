package mpidns.data

import java.net.InetAddress

sealed abstract class RR(ttl: Int, val recType: RecordType) {
  def compress(forest: List[SuffixTree], data: Array[Byte]): (List[SuffixTree], Array[Byte]) = {
    val recTypeBytes = Compression.int16ToBytes(recType.id)
    val classBytes = Compression.int16ToBytes(1)
    val ttlBytes = Compression.int32ToBytes(ttl)
    (forest, data ++ recTypeBytes ++ classBytes ++ ttlBytes)
  }
}

case class RR_A(ttl: Int, addr: InetAddress) extends RR(ttl, A) {
  override def compress(forest: List[SuffixTree], data: Array[Byte]): (List[SuffixTree], Array[Byte]) = {
    val (superForest, superData) = super.compress(forest, data)
    val rdLengthBytes = Compression.int16ToBytes(4)
    val addressBytes = addr.getAddress()
    (superForest, superData ++ rdLengthBytes ++ addressBytes)
  }
}

case class RR_NS(ttl: Int, fqdn: Name, addrs: List[InetAddress]) extends RR(ttl, NS) {
  override def compress(forest: List[SuffixTree], data: Array[Byte]): (List[SuffixTree], Array[Byte]) = {
    val (superForest, superData) = super.compress(forest, data)
    Compression.updateWithUnknownLength(superForest, superData, Compression.addNameToForest(fqdn.fqdn))
  }
}

case class RR_CNAME(ttl: Int, fqdn: Name, child_records: List[(Name, RR)]) extends RR(ttl, CNAME) {
  override def compress(forest: List[SuffixTree], data: Array[Byte]): (List[SuffixTree], Array[Byte]) = {
    val (superForest, superData) = super.compress(forest, data)
    Compression.updateWithUnknownLength(superForest, superData, Compression.addNameToForest(fqdn.fqdn))
  }
  def flatten_extra: List[(Name, RR)] = {
    def do_flatten(crs: List[(Name, RR)], cr: (Name, RR)): List[(Name, RR)] = {
      val (n, rr) = cr
      return (rr match {
        case RR_CNAME(_, _, extra) =>
          (n, rr) :: rr.asInstanceOf[RR_CNAME].flatten_extra ++ crs
        case _ => (n, rr) :: crs
      })
    }
    return child_records.foldLeft(List[(Name, RR)]())(do_flatten)
  }
}

case class RR_SOA(ttl: Int, fqdn: Name, hostmaster: Name, serial: Long,
  refresh: Int, retry: Int, expire: Int, minimum: Int) extends RR(ttl, SOA) {
  override def compress(forest: List[SuffixTree], data: Array[Byte]): (List[SuffixTree], Array[Byte]) = {
    val (superForest, superData) = super.compress(forest, data)
    val (forestWithMName, dataWithMName) =
      Compression.updateWithUnknownLength(superForest, superData, Compression.addNameToForest(fqdn.fqdn))
    val (forestWithRName, dataWithRName) =
      Compression.updateWithUnknownLength(forestWithMName, dataWithMName, Compression.addNameToForest(hostmaster.fqdn))

    val serialBytes = Compression.long32ToBytes(serial)
    val refreshBytes = Compression.int32ToBytes(refresh)
    val retryBytes = Compression.int32ToBytes(retry)
    val expireBytes = Compression.int32ToBytes(expire)
    val minimumBytes = Compression.int32ToBytes(minimum)

    (forestWithRName, dataWithRName ++ serialBytes ++ refreshBytes ++ retryBytes ++ expireBytes ++ minimumBytes)
  }
}

case class RR_PTR(ttl: Int, fqdn: Name) extends RR(ttl, PTR) {
  override def compress(forest: List[SuffixTree], data: Array[Byte]): (List[SuffixTree], Array[Byte]) = {
    val (superForest, superData) = super.compress(forest, data)
    Compression.updateWithUnknownLength(superForest, superData, Compression.addNameToForest(fqdn.fqdn))
  }
}

case class RR_MX(ttl: Int, prio: Int, fqdn: Name) extends RR(ttl, MX) {
  override def compress(forest: List[SuffixTree], data: Array[Byte]): (List[SuffixTree], Array[Byte]) = {
    val (superForest, superData) = super.compress(forest, data)
    val preferenceBytes = Compression.int16ToBytes(prio)
    Compression.updateWithUnknownLength(superForest, superData ++ preferenceBytes, Compression.addNameToForest(fqdn.fqdn))
  }
}

case class RR_TXT(ttl: Int, text: String) extends RR(ttl, TXT) {
  override def compress(forest: List[SuffixTree], data: Array[Byte]): (List[SuffixTree], Array[Byte]) = {
    val (superForest, superData) = super.compress(forest, data)
    val textBytes = text.getBytes()
    val textLengthBytes = Array(textBytes.length.toByte)
    val rdLength = textBytes.length + 1
    val rdLengthBytes = Compression.int16ToBytes(rdLength)
    (superForest, superData ++ rdLengthBytes ++ textLengthBytes ++ textBytes)
  }
}

sealed abstract class PlainRR(ttl: Int)

sealed case class PlainRR_NS(ttl: Int, fqdn: Name) extends PlainRR(ttl)
sealed case class PlainRR_A(ttl: Int, addr: InetAddress) extends PlainRR(ttl)
sealed case class PlainRR_SOA(ttl: Int, fqdn: Name, hostmaster: Name,
  serial: Long, refresh: Int, retry: Int, expire: Int, minimum: Int) extends PlainRR(ttl)
sealed case class PlainRR_PTR(ttl: Int, fqdn: Name) extends PlainRR(ttl)
sealed case class PlainRR_MX(ttl: Int, prio: Int, fqdn: Name) extends PlainRR(ttl)
sealed case class PlainRR_TXT(ttl: Int, text: String) extends PlainRR(ttl)
sealed case class PlainRR_CNAME(ttl: Int, fqdn: Name) extends PlainRR(ttl)
