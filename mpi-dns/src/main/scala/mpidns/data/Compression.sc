package mpidns.data

import scala.collection.mutable.ArrayBuffer

object Compression {
  def ??? : Nothing = throw new Error("an implementation is missing")
                                                  //> ??? : => Nothing
  type LabelOrPointer = Either[String, Int]

  case class SuffixTree(label: String, pointer: Int, children: List[SuffixTree])

  def newForestAccu(name: List[String], relativeTo: Int, accu: List[SuffixTree]): (List[SuffixTree], Int) = name match {
    case List() => (accu, relativeTo)
    case x :: xs => newForestAccu(xs, relativeTo + 1, List(SuffixTree(x, relativeTo, accu)))
  }                                               //> newForestAccu: (name: List[String], relativeTo: Int, accu: List[mpidns.data.
                                                  //| Compression.SuffixTree])(List[mpidns.data.Compression.SuffixTree], Int)

  def newForest(name: List[String], relativeTo: Int) = newForestAccu(name, relativeTo, List())
                                                  //> newForest: (name: List[String], relativeTo: Int)(List[mpidns.data.Compressio
                                                  //| n.SuffixTree], Int)

  newForest(List("mail", "google", "com"), 3)     //> res0: (List[mpidns.data.Compression.SuffixTree], Int) = (List(SuffixTree(com
                                                  //| ,5,List(SuffixTree(google,4,List(SuffixTree(mail,3,List())))))),6)

  def addNameToTree(name: List[String], tree: SuffixTree, data: Array[LabelOrPointer], relativeTo: Int): (SuffixTree, Array[LabelOrPointer], Int, Boolean) = name match {
    case List() => (tree, data, relativeTo, true)
    case x :: xs => tree match {
      case SuffixTree(label, pointer, children) =>
        if (x == label) {
          val (forest, newData, relPos) = addNameToForest(xs, children, data, relativeTo)
          (SuffixTree(label, pointer, forest), newData :+ Right(pointer), relPos + 1, true)
        } else (tree, data, relativeTo, false)
    }
  }                                               //> addNameToTree: (name: List[String], tree: mpidns.data.Compression.SuffixTre
                                                  //| e, data: Array[mpidns.data.Compression.LabelOrPointer], relativeTo: Int)(mp
                                                  //| idns.data.Compression.SuffixTree, Array[mpidns.data.Compression.LabelOrPoin
                                                  //| ter], Int, Boolean)

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
  }                                               //> addNameToForest: (name: List[String], forest: List[mpidns.data.Compression.
                                                  //| SuffixTree], data: Array[mpidns.data.Compression.LabelOrPointer], relativeT
                                                  //| o: Int)(List[mpidns.data.Compression.SuffixTree], Array[mpidns.data.Compres
                                                  //| sion.LabelOrPointer], Int)

  val (forest1, data1, relPos1) = addNameToForest(List("com", "google", "mail"), List(), Array(), 0)
                                                  //> forest1  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(mail,0,List()))))))
                                                  //| data1  : Array[mpidns.data.Compression.LabelOrPointer] = Array(Left(com), L
                                                  //| eft(google), Left(mail))
                                                  //| relPos1  : Int = 3
  val (forest2, data2, relPos2) = addNameToForest(List("com", "google", "calendar"), forest1, data1, relPos1)
                                                  //> forest2  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(mail,0,List()), SuffixTree(calend
                                                  //| ar,3,List()))))))
                                                  //| data2  : Array[mpidns.data.Compression.LabelOrPointer] = Array(Left(com), L
                                                  //| eft(google), Left(mail), Left(calendar), Right(1), Right(2))
                                                  //| relPos2  : Int = 6
  val (forest3, data3, relPos3) = addNameToForest(List("com", "google", "calendar"), forest2, data2, relPos2)
                                                  //> forest3  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(mail,0,List()), SuffixTree(calend
                                                  //| ar,3,List()))))))
                                                  //| data3  : Array[mpidns.data.Compression.LabelOrPointer] = Array(Left(com), L
                                                  //| eft(google), Left(mail), Left(calendar), Right(1), Right(2), Right(3), Righ
                                                  //| t(1), Right(2))
                                                  //| relPos3  : Int = 9
  val (forest4, data4, relPos4) = addNameToForest(List("de", "google"), forest3, data3, relPos3)
                                                  //> forest4  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(mail,0,List()), SuffixTree(calend
                                                  //| ar,3,List()))))), SuffixTree(de,10,List(SuffixTree(google,9,List()))))
                                                  //| data4  : Array[mpidns.data.Compression.LabelOrPointer] = Array(Left(com), L
                                                  //| eft(google), Left(mail), Left(calendar), Right(1), Right(2), Right(3), Righ
                                                  //| t(1), Right(2), Left(de), Left(google))
                                                  //| relPos4  : Int = 11
  val (forest5, data5, relPos5) = addNameToForest(List("de", "google", "mail"), forest4, data4, relPos4)
                                                  //> forest5  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(mail,0,List()), SuffixTree(calend
                                                  //| ar,3,List()))))), SuffixTree(de,10,List(SuffixTree(google,9,List(SuffixTree
                                                  //| (mail,11,List()))))))
                                                  //| data5  : Array[mpidns.data.Compression.LabelOrPointer] = Array(Left(com), L
                                                  //| eft(google), Left(mail), Left(calendar), Right(1), Right(2), Right(3), Righ
                                                  //| t(1), Right(2), Left(de), Left(google), Left(mail), Right(9), Right(10))
                                                  //| relPos5  : Int = 14
}