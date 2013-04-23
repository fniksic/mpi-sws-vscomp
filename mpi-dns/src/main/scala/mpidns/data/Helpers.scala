package mpidns.data

object Helpers {
  def is_soa(rr: RR): Boolean = rr match {
    case RR_SOA(_, _, _, _, _, _, _, _) => return true
    case _ => return false
  }

  def is_ns(rr: RR): Boolean = rr match {
    case RR_NS(_, _, _) => return true
    case _ => return false
  }

  def is_p_soa(rr: PlainRR): Boolean = rr match {
    case PlainRR_SOA(_, _, _, _, _, _, _, _) => return true
    case _ => return false
  }

  def is_p_ns(rr: PlainRR): Boolean = rr match {
    case PlainRR_NS(_, _) => return true
    case _ => return false
  }

  def is_p_cname(rr: PlainRR): Boolean = rr match {
    case PlainRR_CNAME(_, _) => return true
    case _ => return false
  }

}