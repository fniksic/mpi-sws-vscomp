package mpidns

import mpidns.data.AnswerTree
import mpidns.data.Message
import mpidns.data.Header
import mpidns.data.Question
import mpidns.data.PlainRR
import mpidns.data.NO_ERROR
import scala.Array

class Resolver(ans_tree: AnswerTree) {
  
  def resolve(msg: Message): Message = msg match {
    case Message(header:Header,query:Array[Question],answers:Array[PlainRR],
    			 authority:Array[PlainRR],additional: Array[PlainRR]) => {
      var r_answers = Array[PlainRR]()
	  var r_authority = Array[PlainRR]()
	  var r_additional = Array[PlainRR]()
      val authoritative = true;
      val truncated = true;
      val rcode = NO_ERROR()
      val r_header = Header(header.id,true,header.opCode,authoritative,truncated,header.recursionDesired,false,
    		  				header.zero,rcode.id,0,r_answers.length,r_authority.length,r_additional.length)	  
	  Message(r_header,Array[Question](),r_answers,r_authority,r_additional)	    
    }
    case e => throw new IllegalArgumentException(e.toString)
  }

}