package mpidns.data

import scala.collection.mutable.ArrayBuffer

object Compression {
  def ??? : Nothing = throw new Error("an implementation is missing")
  type LabelOrPointer = Either[String, Int]

  case class SuffixTree(label: String, pointer: Int, children: List[SuffixTree])

  def newForestAccu(name: List[String], relativeTo: Int, accu: List[SuffixTree]): (List[SuffixTree], Int) = name match {
    case List() => (accu, relativeTo)
    case x :: xs => newForestAccu(xs, relativeTo + 1, List(SuffixTree(x, relativeTo, accu)))
  }

  def newForest(name: List[String], relativeTo: Int) = newForestAccu(name, relativeTo, List())

  newForest(List("mail", "google", "com"), 3)

  def addNameToTree(name: List[String], tree: SuffixTree, data: Array[LabelOrPointer], relativeTo: Int): (SuffixTree, Array[LabelOrPointer], Int, Boolean) = name match {
    case List() => (tree, data, relativeTo, true)
    case x :: xs => tree match {
      case SuffixTree(label, pointer, children) =>
        if (x == label) {
          val (forest, newData, relPos) = addNameToForest(xs, children, data, relativeTo)
          (SuffixTree(label, pointer, forest), newData :+ Right(pointer), relPos + 1, true)
        } else (tree, data, relativeTo, false)
    }
  }

  def addNameToForest(name: List[String], forest: List[SuffixTree], data: Array[LabelOrPointer], relativeTo: Int): (List[SuffixTree], Array[LabelOrPointer], Int) = forest match {
    case List() => {
      val (frst, relPos) = newForest(name.reverse, relativeTo)
      val liftedName = name map (x => Left(x))
      (frst, data ++ liftedName, relPos)
    }
    case tree :: rest => {
      val (newTree, newData, relPos, added) = addNameToTree(name, tree, data, relativeTo)
      if (added && !newData.isEmpty && newData.last.isLeft) (newTree :: rest, newData :+ Left(""), relPos + 1)
      else if (added) (newTree :: rest, newData, relPos)
      else {
        val (newForest, newerData, newRelPos) = addNameToForest(name, rest, newData, relPos)
        (tree :: newForest, newerData, newRelPos)
      }
    }
  }

  val (forest1, data1, relPos1) = addNameToForest(List("com", "google", "mail"), List(), Array(), 0)
  val (forest2, data2, relPos2) = addNameToForest(List("com", "google", "calendar"), forest1, data1, relPos1)
  val (forest3, data3, relPos3) = addNameToForest(List("com", "google", "calendar"), forest2, data2, relPos2)
  val (forest4, data4, relPos4) = addNameToForest(List("de", "google"), forest3, data3, relPos3)
  val (forest5, data5, relPos5) = addNameToForest(List("de", "google", "mail"), forest4, data4, relPos4)
}