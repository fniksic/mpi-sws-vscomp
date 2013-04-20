package mpidns.data

import java.net.Inet4Address

sealed abstract class PlainRR(ttl: Int)

sealed case class PlainRR_NS(ttl: Int, fqdn: Name) extends PlainRR(ttl)
sealed case class PlainRR_A(ttl: Int, addr: Inet4Address) extends PlainRR(ttl)
sealed case class PlainRR_SOA(ttl: Int, fqdn: Name, hostmaster: String,
    refresh: Int, retry: Int, expire: Int, minimum: Int) extends PlainRR(ttl)
sealed case class PlainRR_PTR(ttl: Int, fqdn: Name) extends PlainRR(ttl)
sealed case class PlainRR_MX(ttl: Int, prio: Int, fqdn: Name) extends PlainRR(ttl)
sealed case class PlainRR_TXT(ttl: Int, text: String) extends PlainRR(ttl)
sealed case class PlainRR_CNAME(ttl: Int, fqdn: Name) extends PlainRR(ttl)

sealed class TreeNode(children:Map[String, TreeNode], rrs: List[PlainRR])

sealed abstract class PlainTree {
  def addRR(name: Name, rr: PlainRR): PlainTree;  
}

object EmptyPlainTree extends PlainTree {
  override def addRR(name: Name, rr: PlainRR) = this;
}