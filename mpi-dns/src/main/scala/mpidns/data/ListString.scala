package mpidns.data

sealed abstract class ListString;
case class Nil extends ListString;
case class Cons(head : String, tail : ListString);
