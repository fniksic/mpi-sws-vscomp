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

	newForest(List("mail", "google", "com"), 3)
                                                  //> res0: (List[mpidns.data.Compression.SuffixTree], Int) = (List(SuffixTree(com
                                                  //| ,5,List(SuffixTree(google,4,List(SuffixTree(mail,3,List())))))),6)
	
  def addNameToTree(name: List[String], tree: SuffixTree, relativeTo: Int): (SuffixTree, Int, Boolean) = name match {
    case List() => (tree, relativeTo, true)
    case x :: xs => tree match {
      case SuffixTree(label, pointer, children) =>
        if (x == label) {
        	val (forest, relPos) = addNameToForest(xs, children, relativeTo)
        	(SuffixTree(label, pointer, forest), relPos, true)
        }
        else (tree, relativeTo, false)
    }
  }                                               //> addNameToTree: (name: List[String], tree: mpidns.data.Compression.SuffixTre
                                                  //| e, relativeTo: Int)(mpidns.data.Compression.SuffixTree, Int, Boolean)

  def addNameToForest(name: List[String], forest: List[SuffixTree], relativeTo: Int): (List[SuffixTree], Int) = forest match {
    case List() => newForest(name.reverse, relativeTo)
    case tree :: rest => {
      val (newTree, relPos, added) = addNameToTree(name, tree, relativeTo)
      if (added) (newTree :: rest, relPos)
      else {
      	val (newForest, newRelPos) = addNameToForest(name, rest, relPos)
      	(tree :: newForest, newRelPos)
    	}
  	}
  }                                               //> addNameToForest: (name: List[String], forest: List[mpidns.data.Compression.
                                                  //| SuffixTree], relativeTo: Int)(List[mpidns.data.Compression.SuffixTree], Int
                                                  //| )

  val (forest1, relPos1) = addNameToForest(List("com", "google", "mail"), List(), 0)
                                                  //> forest1  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(mail,0,List()))))))
                                                  //| relPos1  : Int = 3
	val (forest2, relPos2) = addNameToForest(List("com", "google", "calendar"), forest1, relPos1)
                                                  //> forest2  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(mail,0,List()), SuffixTree(calend
                                                  //| ar,3,List()))))))
                                                  //| relPos2  : Int = 4
  val (forest3, relPos3) = addNameToForest(List("com", "google", "calendar"), forest2, relPos2)
                                                  //> forest3  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(mail,0,List()), SuffixTree(calend
                                                  //| ar,3,List()))))))
                                                  //| relPos3  : Int = 4
  val (forest4, relPos4) = addNameToForest(List("de", "google"), forest3, relPos3)
                                                  //> forest4  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(mail,0,List()), SuffixTree(calend
                                                  //| ar,3,List()))))), SuffixTree(de,5,List(SuffixTree(google,4,List()))))
                                                  //| relPos4  : Int = 6
  val (forest5, relPos5) = addNameToForest(List("de", "google", "mail"), forest4, relPos4)
                                                  //> forest5  : List[mpidns.data.Compression.SuffixTree] = List(SuffixTree(com,2
                                                  //| ,List(SuffixTree(google,1,List(SuffixTree(mail,0,List()), SuffixTree(calend
                                                  //| ar,3,List()))))), SuffixTree(de,5,List(SuffixTree(google,4,List(SuffixTree(
                                                  //| mail,6,List()))))))
                                                  //| relPos5  : Int = 7
}