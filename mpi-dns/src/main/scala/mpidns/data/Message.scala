package mpidns.data

case class Message(
  val header: Header,
  val query: Array[Question],
  val answers: Array[(Name, RR)],
  val authority: Array[(Name, RR)],
  val additional: Array[(Name, RR)])