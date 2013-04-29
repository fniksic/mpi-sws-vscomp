package mpidns.data

import java.net.InetAddress
import scala.collection.immutable.Stack


sealed class TreeNode(Children:Map[String, TreeNode], Rrs: List[PlainRR]) {
  val children = Children
  val rrs = Rrs
}

sealed abstract class PlainTree {
  def getRR(name: Name): Option[List[PlainRR]];
  def addRR(name: Name, rr: PlainRR): PlainTree;
  def dfs[S](visitor: (S, Stack[String], List[PlainRR]) => S,
      visitor2: (S, Stack[String], List[PlainRR]) => S,
      init: S): S;  
  def dfs[S](visitor: (S, Stack[String], List[PlainRR]) => S, init: S): S = {
    def id (s: S, p: Stack[String], r: List[PlainRR]) = s
    return dfs(visitor, id, init)
  }
}

object PlainTreeHelpers {
  def buildPath(fqdn: List[String], rr: PlainRR): TreeNode = {
    var node = new TreeNode(Map(), List(rr))
    for (np <- fqdn) {
      node = new TreeNode(Map(np -> node), List())
    }
    return node
  }
  
  private def buildPlainTreeaddEntry (pt: PlainTree, d: (String, PlainRR)): PlainTree =
    d match { case (n, rr) => return  pt.addRR(new Name(n), rr) }

  def buildPlainTree (zone_records: List[(String, PlainRR)]): PlainTree =
    zone_records.foldLeft(EmptyPlainTree: PlainTree)(buildPlainTreeaddEntry)
}

class NonemptyPlainTree(Root: TreeNode) extends PlainTree {
  val root = Root
  
  override def getRR(name: Name): Option[List[PlainRR]] = {
    def get_node(name: List[String], node: TreeNode): Option[TreeNode] = {
      name match {
        case scala.collection.immutable.Nil => new Some(node)
        case h :: t => get_node(t, node) match {
          case None => None
          case Some(n) => n.children.get(h)
        }
      }
    }
    return (get_node(name.fqdn, root) match {
      case None => None
      case Some(n) => new Some(n.rrs)
    })
  }
  
  private def doDfs[S](visitor: ((S, Stack[String], List[PlainRR]) => S),
      visitor2: ((S, Stack[String], List[PlainRR]) => S),
      pref: Stack[String], node: TreeNode, state: S): S = {
    var s= visitor(state, pref, node.rrs)
    for ((k, v) <- node.children) {
      s = doDfs(visitor, visitor2, pref.push(k), v, s)
    }
    return visitor2(s, pref, node.rrs)
  }
  
  override def dfs[S](visitor: (S, Stack[String], List[PlainRR]) => S,
      visitor2: (S, Stack[String], List[PlainRR]) => S,
      init: S): S = {
    return doDfs(visitor, visitor2, Stack(), root, init)
  }
  
  private def doAddRR(fqdn: List[String], node: TreeNode, rr: PlainRR): TreeNode = {
    fqdn match {
      case scala.collection.immutable.Nil => 
        return new TreeNode(node.children, rr :: node.rrs)
      case head :: tail =>
        val new_child = node.children.get(head) match {
          case None => PlainTreeHelpers.buildPath(fqdn.tail.reverse, rr)
          case Some(child) => doAddRR(tail, child, rr)
        }
        return new TreeNode(node.children - head + (head -> new_child), node.rrs)
    }
  }
  override def addRR(name: Name, rr: PlainRR): PlainTree = {
    return new NonemptyPlainTree(doAddRR(name.fqdn.reverse, root, rr))
  }
}

object EmptyPlainTree extends PlainTree {
  override def dfs[S](visitor: (S, Stack[String], List[PlainRR]) => S,
      visitor2: (S, Stack[String], List[PlainRR]) => S, 
      init: S): S = init
  override def getRR(n: Name): Option[List[PlainRR]] = None
  override def addRR(name: Name, rr: PlainRR): PlainTree = {
    return new NonemptyPlainTree(PlainTreeHelpers.buildPath(name.fqdn, rr))
  }
}
