package mpidns.data

sealed abstract class RR(ttl: Integer) {}

sealed class AnswerTreeNode(children:Map[String, TreeNode], rrs: List[PlainRR]) {}

sealed abstract class AnswerTree(root: AnswerTreeNode) {
    
}