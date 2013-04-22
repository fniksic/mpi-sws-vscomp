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
import mpidns.data.RR_CNAME
import mpidns.data.NAME_ERROR
import mpidns.data.AnswerTree
import mpidns.data.AnswerTree
import mpidns.data.AnswerTree
import mpidns.data.Helpers
import mpidns.data.RR_NS
import mpidns.data.RR_NS

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
      val state: ResolverState = query.foldLeft(new ResolverState(Array[(Name,RR)](),Array[(Name,RR)](),Array[(Name,RR)](),false,NO_ERROR))((state,q) => handle(state,q.qtype,q.qname))
      val r_header = Header(header.id,true,header.opCode,state.authoritative,false,header.recursionDesired,false,header.zero,
    		  				state.rcode.id,0,state.answers.length,state.authorities.length,state.additionals.length)	  
	  Message(r_header,Array[Question](),state.answers,state.authorities,state.additionals)	    
    }
    case e => throw new IllegalArgumentException(e.toString)
  }

  def handle(state: ResolverState, qtype: Either[RecordType, Unit], qname: Name): ResolverState = {
    val (rtype: RecordType, wildcard) = qtype match { case Left(l) => (l, false) case Right(r) => (r, true) }
    if (ans_tree.is_authoritative(qname)) {
      val state_ = state.setAuthoritative(true) // DNS is authoritative for this qname
      ans_tree.get_rrs(qname) match {
        case Some(rrs) => {
          val (cnames, non_cnames) = partition_cnames(rrs)
          if (cnames.isEmpty) {
            state_.addAnswer(filter_rrs(rrs, rtype).map(map_add_name(qname))) // Case 1.1.1
          } else if (cnames.length == 1) {
            rtype match {
              case CNAME => {
                state_.addAnswer(List((qname, cnames(0)))) // Case 1.1.2.2 CNAME
              }
              case x => {
                if (!wildcard) {
                  cnames(0) match {
                    case RR_CNAME(ttl, fqdn, childs) => {
                      val (cn, non_cn) = partition_cnames2((qname, cnames(0)) :: cnames(0).asInstanceOf[RR_CNAME].flatten_extra)
                      state_.addAnswer(cn).addAnswer(filter_rrs2(non_cn, rtype)) // Case 1.1.2.1
                    }
                    case e => throw new IllegalArgumentException(e.toString)
                  }
                } else if (wildcard) {
                  state_.addAnswer(rrs.map(map_add_name(qname))) // Case 1.1.2.2 ANY
                } else {
                  throw new IllegalArgumentException(x.toString)
                }
              }
            }
          } else {
            throw new IllegalArgumentException("Multiple CNAME records")
          }
        }
        case None => state_.setResponseCode(NAME_ERROR) // Case 1.2 including Case 1.2.1 and Case 1.2.2
      }
    } else { // DNS server is not authoritative for this qname
      ans_tree.find_maximal_prefix(qname, Helpers.is_ns) match {
        case Some(p) => { // Case 2.1
          val auth_records = ans_tree.get_rrs(qname) match {
            case Some(ns_rrs) => filter_rrs(ns_rrs, NS)
            case None => throw new IllegalArgumentException("No NS records")
          }
          val state_ = state.addAuthority(auth_records.map(map_add_name(qname)))
          auth_records.foldLeft(state_)({ (state, ns) =>
            ns match {
              case RR_NS(_, fqdn, _) => ans_tree.get_rrs(fqdn) match {
                case Some(rrs) => state_.addAdditionals(filter_rrs(rrs, A).map(map_add_name(qname)))
                case None => throw new IllegalArgumentException("Damn")
              }
              case _ => state_
            }
          })
        }
        case None => state // Case 2.2 
      }
    }
  }
  
  def partition_cnames2(rrs:List[(Name,RR)]) = rrs.partition({ x => x match { case (_,RR_CNAME(_,_,_)) => true case _ => false}})  
  def filter_rrs2(rrs:List[(Name,RR)], rtype: RecordType) = rrs.filter({
    rr => rr match {
      case (_,RR_A(_,_)) if (rtype.id == 1) => true
      case (_,RR_NS(_,_,_)) if (rtype.id == 2) => true
      case (_,RR_SOA(_,_,_,_,_,_,_,_)) if (rtype.id == 6) => true
      case (_,RR_PTR(_,_)) if (rtype.id == 12) => true
      case (_,RR_MX(_,_,_)) if (rtype.id == 15) => true
      case (_,RR_TXT(_,_)) if (rtype.id == 16) => true
      case (_,RR_CNAME(_,_,_)) if (rtype.id == 5) => true
      case _ => false
    }
  })
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
  def map_add_name(name: Name)(rr: RR): (Name,RR) = (name,rr)
  
  /*def collect_cnames(state:ResolverState,rtype:RecordType,name:Name,rr:List[RR]): ResolverState = rr match {
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
  }*/
}