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

sealed class AnswerTreeNode(Children:Map[String, AnswerTreeNode], Rrs: List[RR],
    Authoritative: Boolean) {
  val children = Children
  val rrs = Rrs
  val authoritative = Authoritative
}

sealed abstract class AnswerTree(root: AnswerTreeNode) {
    val node: AnswerTreeNode
    
    private def find_node_for (n: Name): Option[AnswerTreeNode] = {
      var cn = node
      for (ne <- n.fqdn.reverse) {
        cn.children.get(ne) match {
          case None => return None
          case Some(cn2) => cn = cn2
        }
      }
      return new Some(cn)
    }
    
    private def find_maximal_prefix_for (n: Name, p: AnswerTreeNode => Boolean):
    	Option[AnswerTreeNode] = {
      var last: Option[AnswerTreeNode] = None
      var cn = node
      for (ne <- n.fqdn.reverse) {
        if (p(cn)) last = new Some(cn) 
        cn.children.get(ne) match {
          case None => return last
          case Some(cn2) => cn = cn2
        }
      }
      return (if (p(cn)) new Some(cn) else last)      
    }
    
    
    /* Given an AnswerTree A and a FQDN F,
	 * the *containing zone* of F in A is the maximal prefix P of F such
	 * that A[P] has an SOA record.
     */
    def is_soa (rr: RR): Boolean = rr match {
      case RR_SOA(_, _, _, _, _,_, _) => return true
      case _ => return false
    }
    
    def get_containing_zone(n: Name) =
      find_maximal_prefix_for (n, (p: AnswerTreeNode) => p.rrs.exists(is_soa))
    
    /*  Given an AnswerTree A and a FQDN F,
     *  an FQDN F is *authoritative* if: the containing zone of F is P and
     *  for all L that are prefixes of F that strictly contains P, A[L] has no NS record.
     */
    def is_ns (rr: RR): Boolean = rr match {
      case RR_NS(_, _, _) => return true
      case _ => return false
    }
    def is_authoritative(name: Name): Boolean = {
      var is_auth: Boolean = false
      var n: AnswerTreeNode = node
      if (n.rrs.exists(is_soa)) is_auth = true
      for (ne: String <- name.fqdn.reverse) {
        val chld = n.children.get(ne)
        chld match {
          case None => return is_auth
          case Some(cn) =>
            n = cn
            if (n.rrs.exists(is_soa)) is_auth = true
            else if (n.rrs.exists(is_ns)) is_auth = false
        }
      }
      return is_auth
    }

    /* Given an AnswerTree A and a FQDN F,
     * the *zone* of F in A is the containing zone of F in A if F is authoritative in A,
     * and "unknown" otherwise.
     */
    def get_zone(name: Name): Option[AnswerTreeNode] =
      if (is_authoritative(name)) return get_containing_zone(name)
      else return None

    /* Get the RRs belonging to a name. This implements A[F] above. */
    def get_rrs(name: Name): Option[List[RR]] =
      find_node_for(name) match {
        case None => return None
        case Some(node) => return new Some(node.rrs)
      }
    
}