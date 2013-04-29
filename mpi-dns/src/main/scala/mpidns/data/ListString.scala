package mpidns.data

sealed abstract class ListString
case object Nil extends ListString
case class Cons(head: String, tail: ListString) extends ListString
