package mpidns.data

class Message(
  header: Header,
  query: Array[Question],
  answers: Array[PlainRR],
  authority: Array[PlainRR],
  additional: Array[PlainRR])