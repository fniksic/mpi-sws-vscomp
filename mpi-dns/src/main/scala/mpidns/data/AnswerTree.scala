package mpidns.data

import java.net.Inet4Address

sealed abstract class RR(ttl: Int) {}

sealed case class RR_NS(ttl: Int, fqdn: Name, addrs: List[Inet4Address]) extends RR(ttl)
sealed case class RR_A(ttl: Int, addr: Inet4Address) extends RR(ttl)
sealed case class RR_SOA(ttl: Int, fqdn: Name, hostmaster: String,
    refresh: Int, retry: Int, expire: Int, minimum: Int) extends RR(ttl)
sealed case class RR_PTR(ttl: Int, fqdn: Name) extends RR(ttl)
sealed case class RR_MX(ttl: Int, prio: Int, fqdn: Name) extends RR(ttl)
sealed case class RR_TXT(ttl: Int, text: String) extends RR(ttl)
sealed case class RR_CNAME(ttl: Int, fqdn: Name, child_records: List[RR]) extends RR(ttl)

sealed class AnswerTreeNode(children:Map[String, TreeNode], rrs: List[RR],
    authoritative: Boolean) {}

sealed abstract class AnswerTree(root: AnswerTreeNode) {
    val node: AnswerTreeNode
}