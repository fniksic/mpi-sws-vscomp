package mpidns.data

import java.net.InetAddress
import scala.collection.immutable.Stack
import scala.collection.immutable.Set
import scala.Either.LeftProjection
import scala.collection.immutable.Queue

sealed class AnswerTreeNode(val children: Map[String, AnswerTreeNode], val rrs: List[RR],
  val authoritative: Boolean) {
  override def toString: String = {
    children.keySet.toString + ";" + rrs + (if (authoritative) "+" else "-")
  }
}

sealed class AnswerTree(val root: AnswerTreeNode) {
  val node: AnswerTreeNode = root

  private def find_node_for(n: Name): Option[AnswerTreeNode] = {
    var cn = node
    for (ne <- n.fqdn.reverse) {
      cn.children.get(ne) match {
        case None => return None
        case Some(cn2) => cn = cn2
      }
    }
    return new Some(cn)
  }

  def find_maximal_prefix(n: Name, p: RR => Boolean): Option[Name] = {
    var cn = node
    var prefix: List[String] = List[String]()
    var last_name: Option[Name] =
      if (cn.rrs.exists(p)) Some(new Name(prefix)) else None
    for (ne <- n.fqdn.reverse) {
      if (cn.rrs.exists(p)) last_name = Some(new Name(prefix))
      cn.children.get(ne) match {
        case None => return last_name
        case Some(cn2) => cn = cn2
      }
    }
    return last_name
  }

  private def find_maximal_prefix_for(n: Name, p: AnswerTreeNode => Boolean): Option[AnswerTreeNode] = {
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

  def get_containing_zone(n: Name) =
    find_maximal_prefix_for(n, (p: AnswerTreeNode) => p.rrs.exists(Helpers.is_soa))

  /*  Given an AnswerTree A and a FQDN F,
     *  an FQDN F is *authoritative* if: the containing zone of F is P and
     *  for all L that are prefixes of F that strictly contains P, A[L] has no NS record.
     */
  def is_authoritative(name: Name): Boolean = {
    var is_auth: Boolean = false
    var n: AnswerTreeNode = node
    if (n.rrs.exists(Helpers.is_soa)) is_auth = true
    for (ne: String <- name.fqdn.reverse) {
      val chld = n.children.get(ne)
      chld match {
        case None => return is_auth
        case Some(cn) =>
          n = cn
          if (n.rrs.exists(Helpers.is_soa)) is_auth = true
          else if (n.rrs.exists(Helpers.is_ns)) is_auth = false
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

