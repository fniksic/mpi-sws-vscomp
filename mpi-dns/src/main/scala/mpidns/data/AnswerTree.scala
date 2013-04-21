package mpidns.data

import java.net.InetAddress
import scala.collection.immutable.Stack
import scala.collection.immutable.Set
import scala.Either.LeftProjection
import scala.collection.immutable.Queue
import gov.nasa.jpf.jvm.Verify

sealed abstract class RR(ttl: Int) {}

sealed case class RR_NS(ttl: Int, fqdn: Name, addrs: List[InetAddress]) extends RR(ttl)
sealed case class RR_A(ttl: Int, addr: InetAddress) extends RR(ttl)
sealed case class RR_SOA(ttl: Int, fqdn: Name, hostmaster: String, serial: Long,
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

object Helpers {
    def is_soa (rr: RR): Boolean = rr match {
      case RR_SOA(_, _, _, _, _,_, _, _) => return true
      case _ => return false
    }

    def is_ns (rr: RR): Boolean = rr match {
      case RR_NS(_, _, _) => return true
      case _ => return false
    }

    def is_p_soa (rr: PlainRR): Boolean = rr match {
      case PlainRR_SOA(_, _, _, _, _, _,_, _) => return true
      case _ => return false
    }

    def is_p_ns (rr: PlainRR): Boolean = rr match {
      case PlainRR_NS(_, _) => return true
      case _ => return false
    }

    def is_p_cname (rr: PlainRR): Boolean = rr match {
      case PlainRR_CNAME(_, _) => return true
      case _ => return false
    }
}
sealed class AnswerTree(root: AnswerTreeNode) {
    val node: AnswerTreeNode = root
    
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
    
    def get_containing_zone(n: Name) =
      find_maximal_prefix_for (n, (p: AnswerTreeNode) => p.rrs.exists(Helpers.is_soa))
    
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

sealed abstract class Error(val n: Name) {
  val msg: String
}

class ErrorDoubleSOA(n: Name) extends Error(n) { val msg = "Double SOA record" }
class ErrorCNAME(n: Name) extends Error(n) { val msg = "CNAME mixed with other records" }
class ErrorCNAMELoop(n: Name) extends Error(n) { val msg = "CNAME loop" }
class ErrorNS(n: Name) extends Error(n) { val msg = "Lack of valid glue for NS record" }
class ErrorMX(n: Name) extends Error(n) { val msg = "MX pointing to CNAME" }
class ErrorNonauth(n: Name) extends Error(n) { val msg = "Node outside zone and not glue" }
class ErrorNonauthDelegate(n: Name) extends Error(n) { val msg = "Delegation outside zone" }

object BuildAnswerTree {
  case class Analysis(cname: Map[Name, Name], ns_rhs: Set[Name], mx_rhs: Set[Name],
      errors: Set[Error])
  
  def analysis_dfs (a: Analysis, n: Stack[String], rr: List[PlainRR]): Analysis = {
    /* Check 1: A[F] must not contain two SOA records. */
    var new_cname = a.cname
    var new_errs = a.errors
    var new_ns = a.ns_rhs
    var new_mx = a.mx_rhs
    val name = new Name(n.toList.reverse)
    if (rr.count(Helpers.is_p_soa) >= 2)
      new_errs = new_errs +  new ErrorDoubleSOA(name)    
    /* 2. If A[F] contains a CNAME, it must not contain other records. */
    if (rr.exists(Helpers.is_p_cname)) {
      if (rr.size > 1) {
        new_errs = new_errs + new ErrorCNAME(name)
      } else {
        rr match {
          case PlainRR_CNAME(_, cname) :: _ => new_cname = new_cname + (name -> cname)
          case _ => throw new AssertionError("Unreachable")
        }
      }
    }
    /* Ignore 7 for now */
    for (one_rr <- rr) {
      one_rr match {
        case PlainRR_MX(_, _, n) => new_mx = new_mx + n
        case PlainRR_NS(_, n) => new_ns = new_ns + n
        case _ => 
      }
    }
    return new Analysis(new_cname, new_ns, new_mx, new_errs)
  }
  
  def resolve_cnames (cnames: Map[Name, Name]) (plainTree: PlainTree) (progress: Boolean)
    	(defer: List[Name]) (todo: List[Name]) (done: Set[Name]): Option[Set[Error]] = {
      todo match {
        case scala.collection.immutable.Nil =>
        	defer match {
        	  case scala.collection.immutable.Nil =>
        	    return None
        	  case _ =>
        	    if (progress)
        	      return resolve_cnames(cnames)(plainTree)(false)(List())(defer)(done)
        	    def cname_err (n: Name) = new ErrorCNAMELoop(n)
        	    val errs = defer.map(cname_err)
        	    return new Some(errs.toSet)
        	}
        case name :: rest =>
          val cname = cnames.get(name).get
          if (done(cname))
            return resolve_cnames(cnames)(plainTree)(true)(defer)(rest)(done + name)
          else
            if (cnames.keySet(name))
              return resolve_cnames(cnames)(plainTree)(progress)(name :: defer)(rest)(done)
            else {
              return resolve_cnames(cnames)(plainTree)(true)(defer)(rest)(done + name)
            }
         
      }
    }

  
  def map_rr_simple (rr_plain: PlainRR): RR = rr_plain match {
    case PlainRR_A(ttl, addr) => RR_A(ttl, addr)
    case PlainRR_MX(ttl, prio, name) => RR_MX(ttl, prio, name)
    case PlainRR_PTR(ttl, name) => RR_PTR(ttl, name)
    case PlainRR_SOA(ttl, pns, hostmaster, serial, t1, t2, t3, t4) =>
      RR_SOA(ttl, pns, hostmaster, serial, t1, t2, t3, t4)
    case PlainRR_TXT(ttl, text) => RR_TXT(ttl, text)
    case _ => throw new AssertionError("unreachable")
  }

  def extract_addrs (l: List[InetAddress], rr: PlainRR): List[InetAddress] = {
	rr match {
	  case PlainRR_A(_, addr) => return (addr :: l)
	  case _ => return l
	}
  }

  def map_rr_ns (plainTree: PlainTree)(rr_plain: PlainRR): RR = rr_plain match {
    case PlainRR_NS(ttl, name) =>
      val addrs = plainTree.getRR(name) match {
        case Some(rrs) =>
          rrs.foldLeft(List[InetAddress]())(extract_addrs)
        case None => List[InetAddress]()          
      }
      return RR_NS(ttl, name, addrs)
    case _ => throw new AssertionError("unreachable")
  }    

  def extend_map (xfrm: PlainRR => RR) (map: Map[PlainRR, RR]) (plainRRs: Set[PlainRR]): Map[PlainRR, RR] = {
    return plainRRs.foldLeft(map)((map: Map[PlainRR, RR], rr: PlainRR) => map + (rr -> xfrm(rr)))
  }

  def map_names (rrmap: Map[PlainRR, RR])
  	  (map: Map[Name, List[RR]], prefix: Stack[String], rrs: List[PlainRR]): Map[Name, List[RR]] = {
    rrs match {
      case PlainRR_CNAME(_, _) :: _ => return map
      case _ => val mapped_rrs = rrs.map(rrmap)
      	return map + (new Name(prefix.toList.reverse) -> mapped_rrs)
    }
  }

  class State(last_atn: Option[AnswerTreeNode], work: Stack[AnswerTreeNode]) {
    def push(n: String, rr: List[RR], auth: Boolean): State = {
      val new_node = new AnswerTreeNode(Map[String, AnswerTreeNode](), rr, auth)
      if (!work.isEmpty) {
        val whd = work.head
        val wtl = work.tail
        val new_hd = new AnswerTreeNode(whd.children + (n -> new_node), whd.rrs, whd.authoritative)
        return new State(None, wtl.push(new_hd).push(new_node))
      } else {
        return new State(None, work.push(new_node))
      }
    }
    def pop: State = {
      return new State(Some(work.head), work.tail)
    }
    def is_auth: Boolean = {
      if (!work.isEmpty)
        return work.head.authoritative
      else
    	return false
    }
    val last = last_atn
  }
  
  def build_pre (rr_map: Map[Name, List[RR]]) (s: State, pref: Stack[String], rrs: List[PlainRR]): State = {
    val auth =
      if (rrs.exists(Helpers.is_p_soa)) true
      else if (rrs.exists(Helpers.is_p_ns)) false
      else s.is_auth
    return s.push(pref.head, rr_map.get(new Name(pref.toList.reverse)).get, auth)
  }

  def build_post (s: State, pref: Stack[String], rrs: List[PlainRR]): State = {
    return s.pop
  }

  def rec_sat_cnames (map: Map[Name, List[RR]]) (plainTree: PlainTree) (n: Name):
	  Map[Name, List[RR]]= {
    if (map.get(n).isDefined) return map
    val (ttl, point_to) = plainTree.getRR(n) match {
      case Some(PlainRR_CNAME(ttl, cname) :: _) => (ttl, cname)
      case _ => throw new AssertionError("unreachable")
    }
    val finished_map = rec_sat_cnames(map)(plainTree)(point_to)
    val cname_rec = RR_CNAME(ttl, point_to, map.get(point_to).get)
    return (finished_map + (n -> List(cname_rec)))
  }
  def saturate_cnames(map: Map[Name, List[RR]], plainTree: PlainTree, todo: Set[Name]):
	  Map[Name, List[RR]]= {
    var m = map
    for (n <- todo) {
      m = rec_sat_cnames(map)(plainTree)(n)
    }
    return m
  }
  
  def spec_cnames_correct(pt: PlainTree, cnames: Map[Name, Name]): Boolean = ({
    // check forward
    for ((n, cn) <- cnames) {
      pt.getRR(n) match {
        case None => return false
        case Some(l) => if (!l.exists(Helpers.is_p_cname)) return false
      }
    }
    // check backward
    // TODO
    return true
  })

  def spec_nsp_correct(plainTree: PlainTree, ns_pointees: Set[Name]): Boolean = {
    return true
  } 

  def spec_mx_correct(plainTree: PlainTree, mx_pointees: Set[Name]): Boolean = {
    return true
  }

  def spec_soa_correct(plainTree: PlainTree): Boolean = {
    return true
  }

  def spec_cname_correct(plainTree: PlainTree): Boolean = {
    return true
  }

  def build_answer_tree(plainTree: PlainTree): Either[AnswerTree, Set[Error]] = {
    /* iterate through the tree while checking invariants and collecting information. */
    /* 1. A[F] must not contain two SOA records.
     * 2. If A[F] contains a CNAME, it must not contain other records.
     * 3. CNAMEs may not form loops.
     * 4. If A[F] has an NS record pointing to N, N must either be out-of-bailiwick,
     *   or F[N] must have an A record.
     * 5. If A[F] has an MX record pointing to N, N must not have a CNAME record.
     * 6. If we have a node F that is not authoritative, then there must be an NS record
     *   pointing to F.
     * 7. NS records can only be added in zones.
     * 
     * First check: Go through the tree and check 1, 2, 7. Also collect information
     * about CNAME LHS, NS RHS and MX RHS for checking 4, 5, 6.
     * 
     * The check for 3 is done in a later step.
     */
    val (cnames, ns_pointees, mx_pointees, errs) =
      plainTree.DFS(analysis_dfs, new Analysis(Map(), Set(), Set(), Set()))
      match { case Analysis(cn, ns, mx, err) => (cn, ns, mx, err) }
    val outside: Set[Name] = Set() /* Fix this later TODO */
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * n in ns_pointess <-> there is an f s.t. PT[f] has NS n
     * n in mx_pointees <-> there is an f and a p s.t. PT[f] has MX p n
     * err empty <-> PT satisfies 1, 2 (TODO: make it also satisfy 7) 
     */
    assert(spec_cnames_correct(plainTree, cnames))
    assert(spec_nsp_correct(plainTree, ns_pointees))
    assert(spec_mx_correct(plainTree, mx_pointees))
    if (errs.isEmpty) {
      assert(spec_soa_correct(plainTree))
      assert(spec_cname_correct(plainTree))
    }
    /* Now, check 5 and 6. */
    def errmx (name: Name): Error = new ErrorMX(name)
    val mx_errs = errs.union(mx_pointees.intersect(cnames.keySet).map(errmx))
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * n in ns_pointess <-> there is an f s.t. PT[f] has NS n
     * n in mx_pointees <-> there is an f and a p s.t. PT[f] has MX p n
     * err empty <-> PT satisfies 1, 2, 5 (TODO: make it also satisfy 7) 
     */
    def errna (name: Name): Error = new ErrorNonauth(name)
    val na_mx_errs = mx_errs.union(outside.diff(ns_pointees).map(errna))
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * n in ns_pointess <-> there is an f s.t. PT[f] has NS n
     * n in mx_pointees <-> there is an f and a p s.t. PT[f] has MX p n
     * err empty <-> PT satisfies 1, 2, 5 (TODO: make it also satisfy 6, 7) 
     */
    /* Collect NS information */
    def fold_ns (name: Name, data: Map[Name, List[PlainRR]]): Map[Name, List[PlainRR]] = {
      plainTree.getRR(name) match {
        case None => data
        case Some(rrs) => data + (name -> rrs)
      }
    }
    val ns_info: Map[Name, List[PlainRR]] =
      ns_pointees.foldRight(Map[Name, List[PlainRR]]())(fold_ns)
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * n in ns_pointess <-> there is an f s.t. PT[f] has NS n
     * n in mx_pointees <-> there is an f and a p s.t. PT[f] has MX p n
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * err empty <-> PT satisfies 1, 2, 5 (TODO: make it also satisfy 6, 7) 
     */
    /* Resolve CNAMEs */
    val resolve_result = resolve_cnames(cnames)(plainTree)(false)(List())(cnames.keys.toList)(Set())
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * n in ns_pointess <-> there is an f s.t. PT[f] has NS n
     * n in mx_pointees <-> there is an f and a p s.t. PT[f] has MX p n
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * err empty <-> PT satisfies 1, 2, 5 (TODO: make it also satisfy 6, 7)
     * resolve_resule = None => PT satisfies 3
     * resolve_resuls = Some(x) => PT has CNAME loops 
     */
    val all_errs = resolve_result match {
      case Some(extra_errs) => na_mx_errs.union(extra_errs)
      case None => na_mx_errs
    }
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * n in ns_pointess <-> there is an f s.t. PT[f] has NS n
     * n in mx_pointees <-> there is an f and a p s.t. PT[f] has MX p n
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * err empty <-> PT satisfies 1, 2, 3, 5 (TODO: make it also satisfy 6, 7)
     */    
    /* We have now checked 1, 2, 3, 5, 6 and 7. Checking 4 is easier on the final
     * answer tree, so build that now. Also, we ignore 4 for the time being.
     */
    if (all_errs.size > 0)
      return new Right(errs)
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * PT satisfies 1, 2, 3, 5 (TODO: make it also satisfy 6, 7)
     */    
    /* Building the final tree, round 1: Build actual RRs. Start with
     * non-NS, non-CNAME; then, NS; then, CNAME */
    def collect_RRs(rr_set: Set[PlainRR], path: Stack[String], more_rrs: List[PlainRR]):
      Set[PlainRR] = rr_set.union(more_rrs.toSet)      
    val all_rrs = plainTree.DFS(collect_RRs, Set(): Set[PlainRR])
    val (rrs_cname, rrs_not_cname) = all_rrs.partition(Helpers.is_p_cname)
    val (rrs_ns, rrs_other) = rrs_not_cname.partition(Helpers.is_p_ns)
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * PT satisfies 1, 2, 3, 5 (TODO: make it also satisfy 6, 7)
     * rrs_cname = { r: CNAME(t, n) | exists f, r in PT[f] }
     * rrs_ns = { r: NS(t, n) | exists f, r in PT[f] }
     * rrs_cname = { r | exists f, r in PT[f], r neither CNAME nor NS }
     */    
    /* Deal with the non-NS, non-CNAME case */
    val rr_other = extend_map(map_rr_simple)(Map[PlainRR,RR]())(rrs_other)
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * PT satisfies 1, 2, 3, 5 (TODO: make it also satisfy 6, 7)
     * rrs_cname = { r: CNAME(t, n) | exists f, r in PT[f] }
     * rrs_ns = { r: NS(t, n) | exists f, r in PT[f] }
     * rrs_other = { r | exists f, r in PT[f], r neither CNAME nor NS }
     * rr_other = (PlainRR -> RR)|_{rrs_other}
     */
    /* Now map NS */
    val rr_non_cname = extend_map(map_rr_ns(plainTree))(rr_other)(rrs_ns)
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * PT satisfies 1, 2, 3, 5 (TODO: make it also satisfy 6, 7)
     * rrs_cname = { r: CNAME(t, n) | exists f, r in PT[f] }
     * rrs_ns = { r: NS(t, n) | exists f, r in PT[f] }
     * rrs_other = { r | exists f, r in PT[f], r neither CNAME nor NS }
     * rr_non_cname = (PlainRR -> RR)|_{rrs_other \cup rrs_ns}
     */
    /* Map non-CNAME nodes */
    val non_cname_rrs_for_names = plainTree.DFS(map_names(rr_non_cname), Map[Name,List[RR]]())
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * PT satisfies 1, 2, 3, 5 (TODO: make it also satisfy 6, 7)
     * rrs_cname = { r: CNAME(t, n) | exists f, r in PT[f] }
     * rrs_ns = { r: NS(t, n) | exists f, r in PT[f] }
     * rrs_other = { r | exists f, r in PT[f], r neither CNAME nor NS }
     * rr_non_cname = (PlainRR -> RR)|_{rrs_other \cup rrs_ns}
     * non_cname_rrs_for_names = (Name -> RR^*)|_{ { n | PT[f] has no CNAME } }
     */
    /* Extend with CNAME RRset mappings */
    val all_rrs_for_names = saturate_cnames(non_cname_rrs_for_names, 
        plainTree, cnames.keySet)
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * PT satisfies 1, 2, 3, 5 (TODO: make it also satisfy 6, 7)
     * all_rrs_for_names = (Name -> RR^*)
     */        
    /* Finally, build the answer tree */
    val state: State = new State(None, Stack[AnswerTreeNode]())
    val final_state = plainTree.DFS(build_pre(all_rrs_for_names), build_post, state)
    return new Left(new AnswerTree(final_state.last.get))
  }    
}

object Test {
  def main(args: Array[String]) = {
    val pt:PlainTree = Verify.getObject("mpidns.data.PlainTree").asInstanceOf[PlainTree]
    BuildAnswerTree.build_answer_tree(pt) match {
      case Left(_) => println("ok");
      case Right(_) => println("fail");
    }
  }
}