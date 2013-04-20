package mpidns.data

import scala.io.Source

object ZoneFileReader {

  def read(path: String) = {
    try {
      for (line <- Source.fromFile(path).getLines()) {
        println("Parse line: " + line)

      }
    } catch {
      case ex: Exception => print(ex);
    }
  }

  def firstWord(s: String): (String, Int) = s match {
    case "" => ("", 0)
    case t => {
      val head = t.charAt(0)
      if (head == ':') ("", 1)
      else {
        val (tmp, i) = firstWord(t.substring(1))
        (head + tmp, i + 1)
      }
    }
  }

  def mySplit(s: String): ListString = s match {
    case "" => Cons("", Nil())
    case t => {
      val (word, index) = firstWord(t)
      val tail = t.substring(index)
      Cons(word, mySplit(tail))
    }
  }

}