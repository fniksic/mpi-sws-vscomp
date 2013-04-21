package mpidns.data

import scala.io.Source
import java.util.ArrayList
import sun.net.util.IPAddressUtil
import java.net.InetAddress
import java.io.File

object ZoneFileReader {

  private var file: File = _
  private def SERIAL: Long = file.lastModified()

  private val DEFAULT_MINIMUM: Int = 2560
  private def MINIMUM(minimum: String): Int = minimum match {
    case "" => DEFAULT_MINIMUM
    case _ => minimum.toInt
  }
  private val DEFAULT_EXPIRE: Int = 1048576
  private def EXPIRE(expire: String): Int = expire match {
    case "" => DEFAULT_EXPIRE
    case _ => expire.toInt
  }
  private val DEFAULT_RETRY: Int = 2048
  private def RETRY(retry: String): Int = retry match {
    case "" => DEFAULT_RETRY
    case _ => retry.toInt
  }
  private val DEFAULT_REFRESH: Int = 16384
  private def REFRESH(refresh: String): Int = refresh match {
    case "" => DEFAULT_REFRESH
    case _ => refresh.toInt
  }
  private val DEFAULT_TTL: Int = 86400
  private def TTL(ttl: String): Int = ttl match {
    case "" => DEFAULT_TTL
    case _ => ttl.toInt
  }
  private def IP(ip: String): InetAddress = InetAddress.getByAddress(IPAddressUtil.textToNumericFormatV4(ip))
  private def IP2PTR(address: InetAddress): String = {
    InetAddress.getByAddress(address.getAddress().reverse).getHostAddress() + ".in-addr.arpa"
  }

  def read(path: String): List[Tuple2[String, PlainRR]] = {
    file = new File(path)
    var tokens: List[Tuple2[String, PlainRR]] = List()
    try {
      for (line <- Source.fromFile(file).getLines()) {
        println("Parse line: " + line)
        // skip empty lines
        if (line.length() != 0) {
          // skip comments
          if (line.charAt(0) != '#') {
            val tokenized_line: ListString = tokenize(line)
            println("-> tokens: " + tokenized_line)
            tokens = parseTokens(tokenized_line) ::: tokens
          } else {
            println("-> skip comment")
          }
        } else {
          println("-> skip empty line")
        }
      }
    } catch {
      case ex: Exception => print(ex)
    }
    return tokens
  }

  private def parseTokens(tokens: ListString): List[Tuple2[String, PlainRR]] = tokens match {
    case Nil => null
    case Cons(rr, tail) => {
      val rest = Cons(rr.substring(1), tail)
      rr.charAt(0) match {
        case '.' => parseDot(rest)
        case 'Z' => parseZ(rest)
        case '&' => parseAmp(rest)
        case '=' => parseEqual(rest)
        case '+' => parsePlus(rest)
        case '^' => parseHead(rest)
        case '@' => parseAt(rest)
        case ''' => parseApo(rest)
        case 'C' => parseC(rest)
        case e => throw new IllegalArgumentException(e.toString)
      }
    }
  }

  private def parseDot(tokens: ListString): List[Tuple2[String, PlainRR]] = tokens match {
    case Cons(fqdn, Cons(ip, Cons(x, Cons(ttl, Nil)))) =>
      List((fqdn, PlainRR_NS(TTL(ttl), new Name(x))), (fqdn, PlainRR_SOA(TTL(ttl), new Name(x), 
    		  new Name("hostmaster" :: new Name(fqdn).fqdn), SERIAL, DEFAULT_REFRESH, DEFAULT_RETRY, DEFAULT_EXPIRE, DEFAULT_MINIMUM))) :::
        (if (!ip.isEmpty()) List((x, PlainRR_A(TTL(ttl), IP(ip)))) else List())
    case e => throw new IllegalArgumentException(e.toString)
  }

  private def parseZ(tokens: ListString): List[Tuple2[String, PlainRR]] = tokens match {
    case Cons(fqdn, Cons(pms, Cons(hm, Cons(serial, Cons(refresh, Cons(retry, Cons(expire, Cons(minimum, Cons(ttl, Nil))))))))) =>
      List((fqdn, PlainRR_SOA(TTL(ttl), new Name(pms), 
    		  new Name(hm :: new Name(fqdn).fqdn), SERIAL, REFRESH(refresh), RETRY(retry), EXPIRE(expire), MINIMUM(minimum))))
    case e => throw new IllegalArgumentException(e.toString)
  }

  private def parseAmp(tokens: ListString): List[Tuple2[String, PlainRR]] = tokens match {
    case Cons(fqdn, Cons(ip, Cons(x, Cons(ttl, Nil)))) =>
      List((fqdn, PlainRR_NS(TTL(ttl), new Name(x)))) ::: (if (!ip.isEmpty()) List((x, PlainRR_A(TTL(ttl), IP(ip)))) else List())
    case e => throw new IllegalArgumentException(e.toString)
  }

  private def parseEqual(tokens: ListString): List[Tuple2[String, PlainRR]] = tokens match {
    case Cons(fqdn, Cons(ip, Cons(ttl, Nil))) =>
      List((IP2PTR(IP(ip)), PlainRR_PTR(TTL(ttl), new Name(fqdn)))) ::: (if (!ip.isEmpty()) List((fqdn, PlainRR_A(TTL(ttl), IP(ip)))) else List())
    case e => throw new IllegalArgumentException(e.toString)
  }

  private def parsePlus(tokens: ListString): List[Tuple2[String, PlainRR]] = tokens match {
    case Cons(fqdn, Cons(ip, Cons(ttl, Nil))) =>
      if (!ip.isEmpty()) List((fqdn, PlainRR_A(TTL(ttl), IP(ip)))) else List()
    case e => throw new IllegalArgumentException(e.toString)
  }

  private def parseHead(tokens: ListString): List[Tuple2[String, PlainRR]] = tokens match {
    case Cons(fqdn, Cons(p, Cons(ttl, Nil))) =>
      List((p, PlainRR_PTR(TTL(ttl), new Name(fqdn))))
    case e => throw new IllegalArgumentException(e.toString)
  }

  private def parseAt(tokens: ListString): List[Tuple2[String, PlainRR]] = tokens match {
    case Cons(fqdn, Cons(ip, Cons(x, Cons(dist, Cons(ttl, Nil))))) =>
      List((fqdn, PlainRR_MX(TTL(ttl), dist.toInt, new Name(x)))) ::: (if (!ip.isEmpty()) List((x, PlainRR_A(TTL(ttl), IP(ip)))) else List())
    case e => throw new IllegalArgumentException(e.toString)
  }

  private def parseApo(tokens: ListString): List[Tuple2[String, PlainRR]] = tokens match {
    case Cons(fqdn, Cons(s, Cons(ttl, Nil))) =>
      List((fqdn, PlainRR_TXT(TTL(ttl), s)))
    case e => throw new IllegalArgumentException(e.toString)
  }

  private def parseC(tokens: ListString): List[Tuple2[String, PlainRR]] = tokens match {
    case Cons(fqdn, Cons(p, Cons(ttl, Nil))) =>
      List((fqdn, PlainRR_CNAME(TTL(ttl), new Name(p))))
    case e => throw new IllegalArgumentException(e.toString)
  }

  private def tokenize(s: String): ListString = {
    def innerTokenize(s: String, consumed: Boolean): ListString = s match {
      case "" => if (consumed) Cons("", Nil) else Nil
      case t => {
        val (word, index) = firstToken(t)
        val tail = t.substring(index)
        Cons(word, innerTokenize(tail, word.length() != index))
      }
    }
    innerTokenize(s, true)
  }

  private def firstToken(s: String): (String, Int) = s match {
    case "" => ("", 0)
    case t => {
      val head = t.charAt(0)
      if (head == ':') ("", 1)
      else {
        val (tail, i) = firstToken(t.substring(1))
        (head + tail, i + 1)
      }
    }
  }

}