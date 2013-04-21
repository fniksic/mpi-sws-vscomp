package mpidns.data

class Name(val fqdn: List[String]) {
	def this(FQDN: String) = this((FQDN split "\\.").toList)
	override def toString: String = {
	  fqdn match {
	    case scala.collection.immutable.Nil => return "."
	    case h :: t =>
	      return t.foldLeft(h)((a: String, b: String) => a + "." + b)
	  }
	}
	
	def equals(other: Name): Boolean = {
	  return fqdn.corresponds(other.fqdn)((a: String, b: String) => a.equalsIgnoreCase(b))
	}
	
	override def equals(other: Any): Boolean = {
	  return other.isInstanceOf[Name] && equals(other.asInstanceOf[Name])
	}
	
	override def hashCode(): Int = {
	  return fqdn.hashCode
	}
}