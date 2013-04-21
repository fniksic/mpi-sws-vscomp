package mpidns.data

import java.util.StringTokenizer

class Name(FQDN: List[String]) {
	val fqdn = FQDN
	def this(FQDN: String) = this(List.fromArray(FQDN split "\\."))
}