package mpidns.data

import scala.collection.immutable.Stack
import java.net.InetAddress

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
  private case class Analysis(cname: Map[Name, Name], ns_rhs: Set[Name], mx_rhs: Set[Name],
    errors: Set[Error])

  private def analyzePre(a: Analysis, n: Stack[String], rr: List[PlainRR]): Analysis = {
    /* Check 1: A[F] must not contain two SOA records. */
    var new_cname = a.cname
    var new_errs = a.errors
    var new_ns = a.ns_rhs
    var new_mx = a.mx_rhs
    val name = new Name(n.toList)
    if (rr.count(Helpers.is_p_soa) >= 2)
      new_errs = new_errs + new ErrorDoubleSOA(name)
    println("Considering " + n);
    for (rrr <- rr) {
      println("  " + rrr.toString())
    }
    /* 2. If A[F] contains a CNAME, it must not contain other records. */
    /* 2. If A[F] contains a CNAME, it must not contain other records. */
    if (rr.exists(Helpers.is_p_cname)) {
      println("  Found a CNAME, checking (2)")
      if (rr.size > 1) {
        println("  CNAME is not alone - error")
        new_errs = new_errs + new ErrorCNAME(name)
      } else {
        rr match {
          case PlainRR_CNAME(_, cname) :: _ => {
            println("  Store the CNAME - " + name.toString + " -> " + cname.toString)
            new_cname = new_cname + (name -> cname)
          }
          case _ => throw new AssertionError("Unreachable")
        }
      }
    }
    /* Ignore 7 for now */
    for (one_rr <- rr) {
      one_rr match {
        case PlainRR_MX(_, _, n) =>
          {
            println("  Got an MX " + one_rr + ", storing " + n)
            new_mx = new_mx + n
          }
        case PlainRR_NS(_, n) =>
          {
            println("  Got an NS " + one_rr + ", storing " + n)
            new_ns = new_ns + n
          }
        case _ =>
      }
    }
    return new Analysis(new_cname, new_ns, new_mx, new_errs)
  }

  private def checkCNAMES(cnames: Map[Name, Name])(plainTree: PlainTree)(progress: Boolean)(defer: List[Name])(todo: List[Name])(done: Set[Name]): Option[Set[Error]] = {
    todo match {
      case scala.collection.immutable.Nil =>
        defer match {
          case scala.collection.immutable.Nil =>
            return None
          case _ =>
            if (progress)
              return checkCNAMES(cnames)(plainTree)(false)(List())(defer)(done)
            println("Found CNAME loop")
            println("cname map " + cnames)
            println("deferred names: " + defer)
            println("todo names: " + todo)
            println("handled names: " + done)
            def cname_err(n: Name) = new ErrorCNAMELoop(n)
            val errs = defer.map(cname_err)
            return new Some(errs.toSet)
        }
      case name :: rest =>
        val cname = cnames.get(name).get
        if (done(cname))
          return checkCNAMES(cnames)(plainTree)(true)(defer)(rest)(done + name)
        else if (cnames.keySet(cname))
          return checkCNAMES(cnames)(plainTree)(progress)(name :: defer)(rest)(done)
        else {
          return checkCNAMES(cnames)(plainTree)(true)(defer)(rest)(done + name)
        }

    }
  }

  private def buildIPv4AddrList(l: List[InetAddress], rr: PlainRR): List[InetAddress] = {
    rr match {
      case PlainRR_A(_, addr) => return (addr :: l)
      case _ => return l
    }
  }

  private def buildRRmap(xfrm: PlainRR => RR)(map: Map[PlainRR, RR])(plainRRs: Set[PlainRR]): Map[PlainRR, RR] = {
    return plainRRs.foldLeft(map)((map: Map[PlainRR, RR], rr: PlainRR) => map + (rr -> xfrm(rr)))
  }

  private def buildRRmapSimple(rr_plain: PlainRR): RR = rr_plain match {
    case PlainRR_A(ttl, addr) => RR_A(ttl, addr)
    case PlainRR_MX(ttl, prio, name) => RR_MX(ttl, prio, name)
    case PlainRR_PTR(ttl, name) => RR_PTR(ttl, name)
    case PlainRR_SOA(ttl, pns, hostmaster, serial, t1, t2, t3, t4) =>
      RR_SOA(ttl, pns, hostmaster, serial, t1, t2, t3, t4)
    case PlainRR_TXT(ttl, text) => RR_TXT(ttl, text)
    case _ => throw new AssertionError("unreachable")
  }

  private def buildRRmapNS(plainTree: PlainTree)(rr_plain: PlainRR): RR = rr_plain match {
    case PlainRR_NS(ttl, name) =>
      val addrs = plainTree.getRR(name) match {
        case Some(rrs) =>
          rrs.foldLeft(List[InetAddress]())(buildIPv4AddrList)
        case None => List[InetAddress]()
      }
      return RR_NS(ttl, name, addrs)
    case _ => throw new AssertionError("unreachable")
  }

  private def buildNameMap(rrmap: Map[PlainRR, RR])(map: Map[Name, List[RR]], prefix: Stack[String], rrs: List[PlainRR]): Map[Name, List[RR]] = {
    rrs match {
      case PlainRR_CNAME(_, _) :: _ => return map
      case _ =>
        val mapped_rrs = rrs.map(rrmap)
        return map + (new Name(prefix.toList) -> mapped_rrs)
    }
  }

  private def buildNameMapForOneCNAME(map: Map[Name, List[RR]])(plainTree: PlainTree)(n: Name): Map[Name, List[RR]] = {
    println("Getting data for " + n)
    println("  <-" + map)
    println("  -> " + map.get(n))
    if (map.get(n).isDefined) return map
    val (ttl, point_to) = plainTree.getRR(n) match {
      case Some(PlainRR_CNAME(ttl, cname) :: _) => (ttl, cname)
      case _ => return map
    }
    val finished_map = buildNameMapForOneCNAME(map)(plainTree)(point_to)
    val cname_rec = RR_CNAME(ttl, point_to, map.get(point_to).get.map(rr => (point_to, rr)))
    return (finished_map + (n -> List(cname_rec)))
  }

  private def buildNameMapForCNAMES(map: Map[Name, List[RR]], plainTree: PlainTree, todo: Set[Name]): Map[Name, List[RR]] = {
    var m = map
    for (n <- todo) {
      m = buildNameMapForOneCNAME(m)(plainTree)(n)
    }
    return m
  }

  private class PartialNode(val auth: Boolean, val rr: List[RR],
    val children: Map[String, AnswerTreeNode]) {
    def this(auth1: Boolean, rr1: List[RR]) =
      this(auth1, rr1, Map[String, AnswerTreeNode]())
    def add_child(name: String, node: AnswerTreeNode): PartialNode =
      new PartialNode(auth, rr, children + (name -> node))
    def to_answer_tree_node: AnswerTreeNode =
      new AnswerTreeNode(children, rr, auth)
  }
  private class AnswerTreeConstructState(nodes: Stack[PartialNode]) {
    def this() = this(Stack[PartialNode]())
    def push(auth: Boolean, rr: List[RR]) =
      new AnswerTreeConstructState(nodes.push(new PartialNode(auth, rr)))
    def pop(n: String): AnswerTreeConstructState = {
      assert(!nodes.isEmpty)
      val an = nodes.head.to_answer_tree_node
      val stack = nodes.tail
      if (stack.isEmpty) {
        return new AnswerTreeConstructState(stack)
      } else {
        val top = stack.head
        val base = stack.tail
        return new AnswerTreeConstructState(base.push(top.add_child(n, an)))
      }
    }

    def is_auth: Boolean = {
      if (nodes.isEmpty) return false
      return nodes.head.auth
    }

    def get_root: AnswerTreeNode = {
      println("Node stack: " + nodes)
      assert(nodes.size == 1)
      return nodes.head.to_answer_tree_node
    }
  }

  private def answerTreeConstructPre(name_to_rr: Map[Name, List[RR]])(s: AnswerTreeConstructState, pref: Stack[String], rrs: List[PlainRR]): AnswerTreeConstructState = {
    val rrs: List[RR] = name_to_rr.get(new Name(pref.toList)).getOrElse(List[RR]())
    val auth = if (rrs.exists(Helpers.is_soa)) true
    else if (rrs.exists(Helpers.is_ns)) false
    else s.is_auth
    return s.push(auth, rrs)
  }
  private def answerTreeConstructPost(s: AnswerTreeConstructState, pref: Stack[String], rrs: List[PlainRR]): AnswerTreeConstructState = {
    if (pref.isEmpty)
      return s
    return s.pop(pref.head)
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
      plainTree.dfs(analyzePre, new Analysis(Map(), Set(), Set(), Set())) match { case Analysis(cn, ns, mx, err) => (cn, ns, mx, err) }
    val outside: Set[Name] = Set() /* Fix this later TODO */
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * n in ns_pointess <-> there is an f s.t. PT[f] has NS n
     * n in mx_pointees <-> there is an f and a p s.t. PT[f] has MX p n
     * err empty <-> PT satisfies 1, 2 (TODO: make it also satisfy 7) 
     */
    /* Now, check 5 and 6. */
    def errmx(name: Name): Error = new ErrorMX(name)
    val mx_errs = errs.union(mx_pointees.intersect(cnames.keySet).map(errmx))
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * n in ns_pointess <-> there is an f s.t. PT[f] has NS n
     * n in mx_pointees <-> there is an f and a p s.t. PT[f] has MX p n
     * err empty <-> PT satisfies 1, 2, 5 
     */
    def errna(name: Name): Error = new ErrorNonauth(name)
    val na_mx_errs = mx_errs.union(outside.diff(ns_pointees).map(errna))
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * n in ns_pointess <-> there is an f s.t. PT[f] has NS n
     * n in mx_pointees <-> there is an f and a p s.t. PT[f] has MX p n
     * err empty <-> PT satisfies 1, 2, 5 
     */
    /* Collect NS information */
    def fold_ns(name: Name, data: Map[Name, List[PlainRR]]): Map[Name, List[PlainRR]] = {
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
    val resolve_result = checkCNAMES(cnames)(plainTree)(false)(List())(cnames.keys.toList)(Set())
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * n in ns_pointess <-> there is an f s.t. PT[f] has NS n
     * n in mx_pointees <-> there is an f and a p s.t. PT[f] has MX p n
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * err empty <-> PT satisfies 1, 2, 5
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
     * err empty <-> PT satisfies 1, 2, 3, 5
     */
    /* We have now checked 1, 2, 3, 5, 6 and 7. Checking 4 is easier on the final
     * answer tree, so build that now. Also, we ignore 4 for the time being.
     */
    if (all_errs.size > 0) {
      return new Right(all_errs)
    }
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * PT satisfies 1, 2, 3, 5
     */
    /* Building the final tree, round 1: Build actual RRs. Start with
     * non-NS, non-CNAME; then, NS; then, CNAME */
    def collect_RRs(rr_set: Set[PlainRR], path: Stack[String], more_rrs: List[PlainRR]): Set[PlainRR] = rr_set.union(more_rrs.toSet)
    val all_rrs = plainTree.dfs(collect_RRs, Set(): Set[PlainRR])
    val (rrs_cname, rrs_not_cname) = all_rrs.partition(Helpers.is_p_cname)
    val (rrs_ns, rrs_other) = rrs_not_cname.partition(Helpers.is_p_ns)
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * PT satisfies 1, 2, 3, 5
     * rrs_cname = { r: CNAME(t, n) | exists f, r in PT[f] }
     * rrs_ns = { r: NS(t, n) | exists f, r in PT[f] }
     * rrs_cname = { r | exists f, r in PT[f], r neither CNAME nor NS }
     */
    /* Deal with the non-NS, non-CNAME case */
    val rr_other = buildRRmap(buildRRmapSimple)(Map[PlainRR, RR]())(rrs_other)
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * PT satisfies 1, 2, 3, 5
     * rrs_cname = { r: CNAME(t, n) | exists f, r in PT[f] }
     * rrs_ns = { r: NS(t, n) | exists f, r in PT[f] }
     * rrs_other = { r | exists f, r in PT[f], r neither CNAME nor NS }
     * rr_other = (PlainRR -> RR)|_{rrs_other}
     */
    /* Now map NS */
    val rr_non_cname = buildRRmap(buildRRmapNS(plainTree))(rr_other)(rrs_ns)
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * PT satisfies 1, 2, 3, 5
     * rrs_cname = { r: CNAME(t, n) | exists f, r in PT[f] }
     * rrs_ns = { r: NS(t, n) | exists f, r in PT[f] }
     * rrs_other = { r | exists f, r in PT[f], r neither CNAME nor NS }
     * rr_non_cname = (PlainRR -> RR)|_{rrs_other \cup rrs_ns}
     */
    /* Map non-CNAME nodes */
    val non_cname_rrs_for_names = plainTree.dfs(buildNameMap(rr_non_cname), Map[Name, List[RR]]())
    println("Data so far: " + non_cname_rrs_for_names)
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * PT satisfies 1, 2, 3, 5
     * rrs_cname = { r: CNAME(t, n) | exists f, r in PT[f] }
     * rrs_ns = { r: NS(t, n) | exists f, r in PT[f] }
     * rrs_other = { r | exists f, r in PT[f], r neither CNAME nor NS }
     * rr_non_cname = (PlainRR -> RR)|_{rrs_other \cup rrs_ns}
     * non_cname_rrs_for_names = (Name -> RR^*)|_{ { n | PT[f] has no CNAME } }
     */
    /* Extend with CNAME RRset mappings */
    val all_rrs_for_names = buildNameMapForCNAMES(non_cname_rrs_for_names,
      plainTree, cnames.keySet)
    /* SPEC:
     * (a -> b) in cnames <-> PT[a] has CNAME b
     * (n -> l) in ns_info <-> PT[n] has NS p, PT[p] = l 
     * PT satisfies 1, 2, 3, 5
     * all_rrs_for_names = (Name -> RR^*)
     */
    /* Finally, build the answer tree */
    val final_state = plainTree.dfs(answerTreeConstructPre(all_rrs_for_names), answerTreeConstructPost, new AnswerTreeConstructState())
    return new Left(new AnswerTree(final_state.get_root))
  }

}