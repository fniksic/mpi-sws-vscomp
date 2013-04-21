package mpidns.data

import scala.collection.mutable.ArrayBuffer

object Compression {
  type LabelOrPointer = Either[String, Int]

  case class SuffixTree(label: String, pointer: Int, children: List[SuffixTree])

  def newForestAccu(name: List[String], accuForest: List[SuffixTree], accuData: Array[LabelOrPointer], ending: LabelOrPointer): (List[SuffixTree], Array[LabelOrPointer]) = name match {
    case List() => (accuForest, accuData :+ ending)
    case x :: xs => newForestAccu(xs, List(SuffixTree(x, accuData.length, accuForest)), accuData :+ Left(x), ending)
  }                                               //> newForestAccu: (name: List[String], accuForest: List[mpidns.data.Compression
                                                  //| .SuffixTree], accuData: Array[mpidns.data.Compression.LabelOrPointer], endin
                                                  //| g: mpidns.data.Compression.LabelOrPointer)(List[mpidns.data.Compression.Suff
                                                  //| ixTree], Array[mpidns.data.Compression.LabelOrPointer])

  def newForest(name: List[String], data: Array[LabelOrPointer], ending: LabelOrPointer) = newForestAccu(name, List(), data, ending)
                                                  //> newForest: (name: List[String], data: Array[mpidns.data.Compression.LabelOrP
                                                  //| ointer], ending: mpidns.data.Compression.LabelOrPointer)(List[mpidns.data.Co
                                                  //| mpression.SuffixTree], Array[mpidns.data.Compression.LabelOrPointer])

  def addNameToTree(name: List[String], tree: SuffixTree, data: Array[LabelOrPointer], ending: LabelOrPointer) = name match {
    case List() => (tree, data, true)
    case x :: xs => tree match {
      case SuffixTree(label, pointer, children) =>
        if (x == label) {
          val (forest, newData) = addNameToForest_(xs, children, data, Right(pointer))
          (SuffixTree(label, pointer, forest), newData, true)
        } else (tree, data, false)
    }
  }                                               //> addNameToTree: (name: List[String], tree: mpidns.data.Compression.SuffixTre
                                                  //| e, data: Array[mpidns.data.Compression.LabelOrPointer], ending: mpidns.data
                                                  //| .Compression.LabelOrPointer)(mpidns.data.Compression.SuffixTree, Array[mpid
                                                  //| ns.data.Compression.LabelOrPointer], Boolean)

  def addNameToForest_(name: List[String], forest: List[SuffixTree], data: Array[LabelOrPointer], ending: LabelOrPointer): (List[SuffixTree], Array[LabelOrPointer]) = forest match {
    case List() => newForest(name.reverse, data, ending)
    case tree :: rest => {
      val (newTree, newData, added) = addNameToTree(name, tree, data, ending)
      if (added) (newTree :: rest, newData)
      else {
        val (newForest, newerData) = addNameToForest_(name, rest, newData, ending)
        (tree :: newForest, newerData)
      }
    }
  }                                               //> addNameToForest_ : (name: List[String], forest: List[mpidns.data.Compressio
                                                  //| n.SuffixTree], data: Array[mpidns.data.Compression.LabelOrPointer], ending:
                                                  //|  mpidns.data.Compression.LabelOrPointer)(List[mpidns.data.Compression.Suffi
                                                  //| xTree], Array[mpidns.data.Compression.LabelOrPointer])

  def addNameToForest(name: List[String], forest: List[SuffixTree], data: Array[LabelOrPointer]) = addNameToForest_(name, forest, data, Left(""))
                                                  //> addNameToForest: (name: List[String], forest: List[mpidns.data.Compression.
                                                  //| SuffixTree], data: Array[mpidns.data.Compression.LabelOrPointer])(List[mpid
                                                  //| ns.data.Compression.SuffixTree], Array[mpidns.data.Compression.LabelOrPoint
                                                  //| er])

  val (forest1, data1) = addNameToForest(List("com", "google", "calendar"), List(), Array())
                                                  //> forest1  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(calendar,0,List()))))))
                                                  //| data1  : Array[mpidns.data.Compression.LabelOrPointer] = Array(Left(calenda
                                                  //| r), Left(google), Left(com), Left())
  val (forest2, data2) = addNameToForest(List("com", "google", "mail"), forest1, data1)
                                                  //> forest2  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(calendar,0,List()), SuffixTree(ma
                                                  //| il,4,List()))))))
                                                  //| data2  : Array[mpidns.data.Compression.LabelOrPointer] = Array(Left(calenda
                                                  //| r), Left(google), Left(com), Left(), Left(mail), Right(1))
  val (forest3, data3) = addNameToForest(List("com", "google", "calendar"), forest2, data2)
                                                  //> forest3  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(calendar,0,List()), SuffixTree(ma
                                                  //| il,4,List()))))))
                                                  //| data3  : Array[mpidns.data.Compression.LabelOrPointer] = Array(Left(calenda
                                                  //| r), Left(google), Left(com), Left(), Left(mail), Right(1), Right(0))
  val (forest4, data4) = addNameToForest(List("de", "google"), forest3, data3)
                                                  //> forest4  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(calendar,0,List()), SuffixTree(ma
                                                  //| il,4,List()))))), SuffixTree(de,8,List(SuffixTree(google,7,List()))))
                                                  //| data4  : Array[mpidns.data.Compression.LabelOrPointer] = Array(Left(calenda
                                                  //| r), Left(google), Left(com), Left(), Left(mail), Right(1), Right(0), Left(g
                                                  //| oogle), Left(de), Left())
  val (forest5, data5) = addNameToForest(List("de", "google", "mail"), forest4, data4)
                                                  //> forest5  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(calendar,0,List()), SuffixTree(ma
                                                  //| il,4,List()))))), SuffixTree(de,8,List(SuffixTree(google,7,List(SuffixTree(
                                                  //| mail,10,List()))))))
                                                  //| data5  : Array[mpidns.data.Compression.LabelOrPointer] = Array(Left(calenda
                                                  //| r), Left(google), Left(com), Left(), Left(mail), Right(1), Right(0), Left(g
                                                  //| oogle), Left(de), Left(), Left(mail), Right(7))
}