package mpidns.data

abstract class ResponseCode(val id: Int)

case object NO_ERROR extends ResponseCode(0)
case object FORMAT_ERROR extends ResponseCode(1)
case object SERVER_FAILURE extends ResponseCode(2)
case object NAME_ERROR extends ResponseCode(3)
case object NOT_IMPLEMENTED extends ResponseCode(4)
case object REFUSED extends ResponseCode(5)

object ResponseCode {

  private val idMap =
    Array(NO_ERROR, FORMAT_ERROR, SERVER_FAILURE, NAME_ERROR, NOT_IMPLEMENTED, REFUSED)
      .foldLeft(Map[Int, ResponseCode]())((accu, responseCode) => accu + (responseCode.id -> responseCode))

  def withId(id: Int) = idMap(id)

}