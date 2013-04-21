package mpidns.data

class Name(FQDN: List[String]) {
	val fqdn = FQDN
	def this(FQDN: String) = this(List.fromArray(FQDN split "\\."))
}