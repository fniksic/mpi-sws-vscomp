package mpidns.data

object Compression {
  // TODO: Make sure these don't extend the message outside the limit
  def labelToBytes(x: String) = x.length().toByte +: x.getBytes()

  def int16ToBytes(x: Int) =
    Array(
      ((x & 0xff00) >>> 8).toByte,
      (x & 0xff).toByte)

  def int32ToBytes(x: Int) =
    Array(
      ((x & 0xff000000) >>> 24).toByte,
      ((x & 0xff0000) >>> 16).toByte,
      ((x & 0xff00) >>> 8).toByte,
      (x & 0xff).toByte)

  def long32ToBytes(x: Long) =
    Array(
      ((x & 0xff000000L) >>> 24).toByte,
      ((x & 0xff0000L) >>> 16).toByte,
      ((x & 0xff00L) >>> 8).toByte,
      (x & 0xffL).toByte)

  def bytesToInt16(data: Array[Byte]) = data(0) << 8 | data(1)
  def bytesToInt32(data: Array[Byte]) = data(0) << 24 | data(1) << 16 | data(2) << 8 | data(3)
  def bytesToLong32(data: Array[Byte]) = data(0).toLong << 24 | data(1).toLong << 16 | data(2).toLong << 8 | data(3).toLong

  def pointerToBytes(pointer: Int) = {
    val tmp = int16ToBytes(pointer)
    Array((tmp(0) | 0xc0).toByte, tmp(1))
  }

  def newForestAccu(name: List[String], accuForest: List[SuffixTree], accuData: Array[Byte], ending: Array[Byte]): (List[SuffixTree], Array[Byte]) = name match {
    case List() => (accuForest, accuData ++ ending)
    case x :: xs => newForestAccu(xs, List(SuffixTree(x, accuData.length, accuForest)), accuData ++ labelToBytes(x), ending)
  }

  def newForest(name: List[String], data: Array[Byte], ending: Array[Byte]) = newForestAccu(name, List(), data, ending)

  def addNameToTree(reverseName: List[String], tree: SuffixTree, data: Array[Byte], ending: Array[Byte]) = reverseName match {
    case List() => (tree, data, true)
    case x :: xs => tree match {
      case SuffixTree(label, pointer, children) =>
        if (x == label) {
          val (forest, newData) = addNameToForest_(xs, children, data, pointerToBytes(pointer))
          (SuffixTree(label, pointer, forest), newData, true)
        } else (tree, data, false)
    }
  }

  def addNameToForest_(reverseName: List[String], forest: List[SuffixTree], data: Array[Byte], ending: Array[Byte]): (List[SuffixTree], Array[Byte]) = forest match {
    case List() => newForest(reverseName.reverse, data, ending)
    case tree :: rest => {
      val (newTree, newData, added) = addNameToTree(reverseName, tree, data, ending)
      if (added) (newTree :: rest, newData)
      else {
        val (newForest, newerData) = addNameToForest_(reverseName, rest, newData, ending)
        (tree :: newForest, newerData)
      }
    }
  }

  def addNameToForest(name: List[String])(forest: List[SuffixTree], data: Array[Byte]) = addNameToForest_(name.reverse, forest, data, labelToBytes(""))

  def updateWithUnknownLength(forest: List[SuffixTree], data: Array[Byte], updateFunc: (List[SuffixTree], Array[Byte]) => (List[SuffixTree], Array[Byte])) = {
    val dummyLengthBytes = Array(0.toByte, 0.toByte)
    val dummyLengthPos = data.length
    val (forestWithNewData, dataWithNewData) = updateFunc(forest, data)
    val length = dataWithNewData.length - dummyLengthPos - 2
    val lengthBytes = Compression.int16ToBytes(length)
    dataWithNewData(dummyLengthPos) = lengthBytes(0)
    dataWithNewData(dummyLengthPos + 1) = lengthBytes(1)
    (forestWithNewData, dataWithNewData)
  }

  def handleQuestion(accu: (List[SuffixTree], Array[Byte]), question: Question): (List[SuffixTree], Array[Byte]) = {
    val (forest, data) = accu
    val (newForest, newData) = addNameToForest(question.qname.fqdn)(forest, data)
    val qtype = question.qtype match {
      case Left(recType) => recType.id
      case Right(_) => 255
    }
    (newForest, newData ++ int16ToBytes(qtype) ++ int16ToBytes(question.qclass))
  }

  def handleRR(accu: (List[SuffixTree], Array[Byte]), namedRR: (Name, RR)): (List[SuffixTree], Array[Byte]) = {
    val (name, rr) = namedRR
    val (forest, data) = accu
    val (forestWithName, dataWithName) = addNameToForest(name.fqdn)(forest, data)
    rr.compress(forestWithName, dataWithName)
  }

  def messageToBytes(message: Message): Array[Byte] = {
    val header = headerToBytes(message.header)

    val (forestWithQuestions, dataWithQuestions) = message.query.foldLeft((List[SuffixTree](), header))(handleQuestion)
    val (forestWithAnswers, dataWithAnswers) = message.answers.foldLeft((forestWithQuestions, dataWithQuestions))(handleRR)
    val (forestWithAuthority, dataWithAuthority) = message.authority.foldLeft((forestWithAnswers, dataWithAnswers))(handleRR)
    val (forestWithAdditional, dataWithAdditional) = message.additional.foldLeft((forestWithAuthority, dataWithAuthority))(handleRR)

    dataWithAdditional
  }

  def headerToBytes(header: Header) = {
    val idBytes = int16ToBytes(header.id)

    val response = if (header.response) 0x80 else 0x00
    val opCode = header.opCode << 3
    val authoritative = if (header.authoritative) 0x04 else 0x00
    val truncated = if (header.truncated) 0x02 else 0x00
    val recursionDesired = if (header.recursionDesired) 0x01 else 0x00
    val bunchByte = (response | opCode | authoritative | truncated | recursionDesired).toByte

    val recursionAvailable = if (header.recursionAvailable) 0x80 else 0x00
    val zero = header.zero << 4
    val rCode = header.rCode
    val raZeroRCodeByte = (recursionAvailable | zero | rCode).toByte

    val questionCountBytes = int16ToBytes(header.questionCount)
    val answerCountBytes = int16ToBytes(header.answerCount)
    val authorityCountBytes = int16ToBytes(header.authorityCount)
    val additionalCountBytes = int16ToBytes(header.additionalCount)

    idBytes ++ Array(bunchByte, raZeroRCodeByte) ++ questionCountBytes ++ answerCountBytes ++ authorityCountBytes ++ additionalCountBytes
  }

  def isPointer(byte: Byte) = (byte & 0xc0) != 0

  def getPointer(first: Byte, second: Byte) = first << 8 | second

  def getLabelLength(byte: Byte) = byte & 0x3f

  def extractLabel(data: Array[Byte], start: Int, length: Int) = new String(data.slice(start, start + length))

  def extractName_(data: Array[Byte], start: Int, accu: List[String], visited: Set[Int], next: Option[Int]): (List[String], Int) = {
    if (isPointer(data(start))) {
      val pointer = getPointer(data(start), data(start + 1))
      val newNext = next orElse Some(start + 2)
      if (visited contains pointer) (accu, newNext.get)
      else extractName_(data, pointer, accu, visited + pointer, newNext)
    } else {
      val labelLength = getLabelLength(data(start))
      val newNext = next orElse Some(start + labelLength + 1)
      if (labelLength == 0) (accu, newNext.get)
      else extractName_(data, start + labelLength, extractLabel(data, start, labelLength) :: accu, visited, newNext)
    }
  }

  def extractName(data: Array[Byte], start: Int) = extractName_(data, start, List(), Set(), None)

  def bytesToHeader(data: Array[Byte]): Header = {
    val id = bytesToInt16(data.slice(0, 2))

    val bunchByte = data(2)
    val response = (bunchByte & 0x80) != 0
    val opCode = (bunchByte & 0x78) >>> 3
    val authoritative = (bunchByte & 0x04) != 0
    val truncated = (bunchByte & 0x02) != 0
    val recursionDesired = (bunchByte & 0x01) != 0

    val raZeroRCodeByte = data(3)
    val recursionAvailable = (raZeroRCodeByte & 0x80) != 0
    val zero = (raZeroRCodeByte & 0x70) >>> 4
    val rCode = raZeroRCodeByte & 0x0f

    val questionCount = bytesToInt16(data.slice(4, 6))
    val answerCount = bytesToInt16(data.slice(6, 8))
    val authorityCount = bytesToInt16(data.slice(8, 10))
    val additionalCount = bytesToInt16(data.slice(10, 12))

    Header(id, response, opCode, authoritative, truncated, recursionDesired,
      recursionAvailable, zero, rCode,
      questionCount, answerCount, authorityCount, additionalCount)
  }

  def extractQuestion(data: Array[Byte], start: Int): (Question, Int) = {
    val (qname, offsetAfterName) = extractName(data, start)
    val qtypeNum = bytesToInt16(data.slice(offsetAfterName, offsetAfterName + 2))
    val qtype = if (qtypeNum == 255) Right(()) else Left(RecordType withId qtypeNum)
    val qclass = bytesToInt16(data.slice(offsetAfterName + 2, offsetAfterName + 4))
    
    (Question(new Name(qname), qtype, qclass), offsetAfterName + 4)
  }
  
  def extractNQuestions_(data: Array[Byte], start: Int, n: Int, accu: List[Question]): (List[Question], Int) =
    if (n == 0) (accu, start)
    else {
      val (question, offset) = extractQuestion(data, start)
      extractNQuestions_(data, offset, n - 1, question :: accu)
    }
  
  def extractNQuestions(data: Array[Byte], start: Int, n: Int) = {
    val (questions, offset) = extractNQuestions_(data, start, n, List())
    (questions.reverse, offset)
  }
  
  def extractNRRs_(data: Array[Byte], start: Int, n: Int, accu: List[(Name, RR)]): (List[(Name, RR)], Int) = {
    (List(), 0)
  }
  
  def extractNRRs(data: Array[Byte], start: Int, n: Int) = {
    val (rrs, offset) = extractNRRs_(data, start, n, List())
    (rrs.reverse, offset)
  }
  
  def bytesToMessage(data: Array[Byte]): Message = {
    val header = bytesToHeader(data)
    val (query, offsetAfterQuery) = extractNQuestions(data, 12, header.questionCount)
    val (answers, offsetAfterAnswers) = extractNRRs(data, offsetAfterQuery, header.answerCount)
    val (authority, offsetAfterAuthority) = extractNRRs(data, offsetAfterAnswers, header.authorityCount)
    val (additional, offsetAfterAdditional) = extractNRRs(data, offsetAfterAuthority, header.additionalCount)
    
    Message(header, query.toArray, answers.toArray, authority.toArray, additional.toArray)
  }

}