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

class Resolver(ans_tree: AnswerTree) {
  
  class ResolverState(val answers: Array[PlainRR],
		  			  val authorities: Array[PlainRR],
		  			  val additionals: Array[PlainRR],
		  			  val authoritative: Boolean,
		  			  val rcode: ResponseCode) {
    def addAnswer(answer: List[PlainRR]) = new ResolverState(answers ++ answer, authorities, additionals, authoritative, rcode)
    def addAuthority(answer: List[PlainRR]) = new ResolverState(answers, authorities ++ answer, additionals, authoritative, rcode)
    def addAdditionals(answer: List[PlainRR]) = new ResolverState(answers, authorities, additionals ++ answer, authoritative, rcode)
    def setAuthoritative(auth: Boolean) = new ResolverState(answers, authorities, additionals, auth, rcode)
    def setResponseCode(rc: ResponseCode) = new ResolverState(answers, authorities, additionals, authoritative, rc)
  }
  
  def resolve(msg: Message): Message = msg match {
    case Message(header:Header,query:Array[Question],answers:Array[PlainRR],
    			 authority:Array[PlainRR],additional: Array[PlainRR]) => {
      val state: ResolverState = query.foldLeft(new ResolverState(Array[PlainRR](),Array[PlainRR](),Array[PlainRR](),true,NO_ERROR()))((state,q) => handle(state,q.qtype,q.qname))
      val r_header = Header(header.id,true,header.opCode,state.authoritative,false,header.recursionDesired,false,header.zero,
    		  				state.rcode.id,0,state.answers.length,state.authorities.length,state.additionals.length)	  
	  Message(r_header,Array[Question](),state.answers,state.authorities,state.additionals)	    
    }
    case e => throw new IllegalArgumentException(e.toString)
  }
  
  def handle(state: ResolverState, qtype: Either[RecordType, Unit], name: Name) : ResolverState = {
    val (rtype:RecordType,wildcard) = qtype match { case Left(l) => (l,false) case Right(r) => (r,true) }
    if (ans_tree.is_authoritative(name)) {
      ans_tree.get_rrs(name) match {
        case Some(rrs) => {
          val (cnames,non_cnames) = partition_cnames(rrs)
          if (cnames.isEmpty) {
            state.addAnswer(filter_rrs(rrs, rtype).map(map_simple_to_rr))
          } else {
            state
          }
        }
        case None => state
      }
    } else {
      state
    }
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
  def map_simple_to_rr(rr: RR): PlainRR = rr match {
    case RR_A(ttl, addr) => PlainRR_A(ttl, addr)
    case RR_MX(ttl, prio, name) => PlainRR_MX(ttl, prio, name)
    case RR_PTR(ttl, name) => PlainRR_PTR(ttl, name)
    case RR_SOA(ttl, pns, hostmaster, serial, t1, t2, t3, t4) => 
      PlainRR_SOA(ttl, pns, hostmaster, serial, t1, t2, t3, t4)
    case RR_TXT(ttl, text) => PlainRR_TXT(ttl, text)
    case _ => throw new IllegalArgumentException("unreachable")
  }
}