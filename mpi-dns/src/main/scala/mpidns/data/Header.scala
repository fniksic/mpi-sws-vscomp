package mpidns.data

class Header(
  id: Int,
  response: Boolean,
  opCode: Int,
  authoritative: Boolean,
  truncated: Boolean,
  recursionDesired: Boolean,
  recursionAvailable: Boolean,
  zero: Int = 0,
  rCode: Int,
  questionCount: Int,
  answerCount: Int,
  authorityCount: Int,
  additionalCount: Int)