package mpidns.data

sealed abstract class ResponseCode(id: Int)

case class NO_ERROR extends ResponseCode(0)
case class FORMAT_ERROR extends ResponseCode(1)
case class SERVER_FAILURE extends ResponseCode(2)
case class NAME_ERROR extends ResponseCode(3)
case class NOT_IMPLEMENTED extends ResponseCode(4)
case class REFUSED extends ResponseCode(5)
