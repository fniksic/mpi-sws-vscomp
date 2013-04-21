package mpidns.data

case class SuffixTree(label: String, pointer: Int, children: List[SuffixTree])
