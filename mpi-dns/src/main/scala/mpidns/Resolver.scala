package mpidns

import mpidns.data.AnswerTree
import mpidns.data.Message
import mpidns.data.Header
import mpidns.data.Question
import mpidns.data.PlainRR
import mpidns.data.NO_ERROR
import scala.Array
import mpidns.data.{Name,RecordType}
import mpidns.data.{A,NS,CNAME,SOA,PTR,MX,TXT}
import mpidns.data.ResponseCode
import mpidns.data.RR_CNAME
import mpidns.data.RR
import mpidns.data.RR_A
import mpidns.data.RR_NS
import mpidns.data.RR_SOA
import mpidns.data.RR_PTR
import mpidns.data.RR_MX
import mpidns.data.RR_TXT
import mpidns.data.RR_CNAME
import mpidns.data.PlainRR_SOA
import mpidns.data.PlainRR_A
import mpidns.data.PlainRR_MX
import mpidns.data.PlainRR_PTR
import mpidns.data.PlainRR_TXT
import mpidns.data.RR_A
import scala.collection.immutable.Nil

class Resolver(ans_tree: AnswerTree) {
  
  class ResolverState(val answers: Array[(Name,RR)],
		  			  val authorities: Array[(Name,RR)],
		  			  val additionals: Array[(Name,RR)],
		  			  val authoritative: Boolean,
		  			  val rcode: ResponseCode) {
    def addAnswer(answer: List[(Name,RR)]) = new ResolverState(answers ++ answer, authorities, additionals, authoritative, rcode)
    def addAuthority(answer: List[(Name,RR)]) = new ResolverState(answers, authorities ++ answer, additionals, authoritative, rcode)
    def addAdditionals(answer: List[(Name,RR)]) = new ResolverState(answers, authorities, additionals ++ answer, authoritative, rcode)
    def setAuthoritative(auth: Boolean) = new ResolverState(answers, authorities, additionals, auth, rcode)
    def setResponseCode(rc: ResponseCode) = new ResolverState(answers, authorities, additionals, authoritative, rc)
  }
  
  def resolve(msg: Message): Message = msg match {
    case Message(header:Header,query:Array[Question],answers:Array[(Name,RR)],
    			 authority:Array[(Name,RR)],additional: Array[(Name,RR)]) => {
      val state: ResolverState = query.foldLeft(new ResolverState(Array[(Name,RR)](),Array[(Name,RR)](),Array[(Name,RR)](),true,NO_ERROR))((state,q) => handle(state,q.qtype,q.qname))
      val r_header = Header(header.id,true,header.opCode,state.authoritative,false,header.recursionDesired,false,header.zero,
    		  				state.rcode.id,0,state.answers.length,state.authorities.length,state.additionals.length)	  
	  Message(r_header,Array[Question](),state.answers,state.authorities,state.additionals)	    
    }
    case e => throw new IllegalArgumentException(e.toString)
  }

  def handle(state: ResolverState, qtype: Either[RecordType, Unit], name: Name): ResolverState = {
    val (rtype: RecordType, wildcard) = qtype match { case Left(l) => (l, false) case Right(r) => (r, true) }
    if (ans_tree.is_authoritative(name)) {
      ans_tree.get_rrs(name) match {
        case Some(rrs) => {
          val (cnames, non_cnames) = partition_cnames(rrs)
          if (cnames.isEmpty) {
            state.addAnswer(filter_rrs(rrs, rtype).map(map_enhance_name(name)))
          } else if (cnames.length == 1) {
            rtype match {
              case CNAME => throw new IllegalArgumentException("Illegal RecordType")
              case _ => {
                if (!wildcard) {
                  cnames(0) match {
                    case RR_CNAME(ttl,fqdn,childs) => collect_cnames(state.addAnswer(List((name,cnames(0)))),rtype, fqdn, childs)
                    case e => throw new IllegalArgumentException(e.toString)
                  }
                } else {
                  throw new IllegalArgumentException("Illegal RecordType")
                }
              }
            }
          } else {
            throw new IllegalArgumentException("Multiple CNames")
          }
        }
        case None => state
      }
    } else {
      state
    }
  }
  
  def collect_cnames(state:ResolverState,rtype:RecordType,name:Name,rr:List[RR]): ResolverState = rr match {
    case RR_CNAME(ttl,fqdn,childs)::xs => collect_cnames(state.addAnswer(List((name,RR_CNAME(ttl,fqdn,childs)))),rtype,fqdn,xs)
    case x::xs => state.addAnswer(List((name, x match {
												case RR_A(_,_) if (rtype.id == 1) => x
												case RR_NS(_,_,_) if (rtype.id == 2) => x
												case RR_SOA(_,_,_,_,_,_,_,_) if (rtype.id == 6) => x
												case RR_PTR(_,_) if (rtype.id == 12) => x
												case RR_MX(_,_,_) if (rtype.id == 15) => x
												case RR_TXT(_,_) if (rtype.id == 16) => x
												case RR_CNAME(_,_,_) if (rtype.id == 5) => x
												case x => throw new IllegalArgumentException(x.toString)
											  })))
    case scala.collection.immutable.Nil => throw new IllegalArgumentException("Empty CNames list") 
    case x => throw new IllegalArgumentException(x.toString)
  }
  
  def partition_cnames(rrs:List[RR]) = rrs.partition({ x => x match { case RR_CNAME(_,_,_) => true case _ => false}})
  def filter_rrs(rrs:List[RR], rtype: RecordType) = rrs.filter({
    rr => rr match {
      case RR_A(_,_) if (rtype.id == 1) => true
      case RR_NS(_,_,_) if (rtype.id == 2) => true
      case RR_SOA(_,_,_,_,_,_,_,_) if (rtype.id == 6) => true
      case RR_PTR(_,_) if (rtype.id == 12) => true
      case RR_MX(_,_,_) if (rtype.id == 15) => true
      case RR_TXT(_,_) if (rtype.id == 16) => true
      case RR_CNAME(_,_,_) if (rtype.id == 5) => true
      case _ => false
    }
  })
  def map_enhance_name(name: Name)(rr: RR): (Name,RR) = (name,rr)
}