package mpidns.data

object Compression {
  // TODO: Make sure these don't extend the message outside the limit
  def labelToBytes(x: String) = x.length().toByte +: x.getBytes()

  def int16ToBytes(x: Int) = {
    val firstByte = ((x & 0xff00) >> 8).toByte
    val secondByte = (x & 0xff).toByte
    Array(firstByte, secondByte)
  }

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

  def addNameToForest(name: List[String], forest: List[SuffixTree], data: Array[Byte]) = addNameToForest_(name.reverse, forest, data, labelToBytes(""))

  def handleQuestion(accu: (List[SuffixTree], Array[Byte]), question: Question): (List[SuffixTree], Array[Byte]) = {
  	val (forest, data) = accu
  	val (newForest, newData) = addNameToForest(question.qname.fqdn, forest, data)
  	val qtype = question.qtype match {
  		case Left(recType) => recType.id
  		case Right(_) => 255
  	}
  	(newForest, newData ++ int16ToBytes(qtype) ++ int16ToBytes(question.qclass))
  }                                               //> handleQuestion: (accu: (List[mpidns.data.Compression.SuffixTree], Array[Byt
                                                  //| e]), question: mpidns.data.Question)(List[mpidns.data.Compression.SuffixTre
                                                  //| e], Array[Byte])
  
  def handleRR(accu: (List[SuffixTree], Array[Byte]), namedRR: (Name, RR)): (List[SuffixTree], Array[Byte]) = {
  	val (forest, data) = accu
  	
  }
		
  def messageToBytes(message: Message): Array[Byte] = {
  	val header = headerToBytes(message.header)
  	
  	val (forestWithQuestions, dataWithQuestions) = message.query.foldLeft((List[SuffixTree](), header))(handleQuestion)
  	val (forestWithAnswers, dataWithAnswers) = message.answers.foldLeft((forestWithQuestions, dataWithQuestions))(handleRR)
  	val (forestWithAuthority, dataWithAuthority) = message.authority.foldLeft((forestWithAnswers, dataWithAnswers))(handleRR)
  	val (forestWithAdditional, dataWithAdditional) = message.additional.foldLeft((forestWithAuthority, dataWithAuthority))(handleRR)
  	
  	dataWithAdditional
  }                                               //> messageToBytes: (message: mpidns.data.Message)Array[Byte]
    
  def headerToBytes(header: Header) = {
  	val idBytes = int16ToBytes(header.id)
  	
  	val response = if (header.response) 0x80 else 0x00
  	val opCode = header.opCode << 4
  	val authoritative = if (header.authoritative) 0x08 else 0x00
  	val truncated = if (header.truncated) 0x04 else 0x00
  	val recursionDesired = if (header.recursionDesired) 0x02 else 0x00
  	val recursionAvailable = if (header.recursionAvailable) 0x01 else 0x00
  	val bunchByte = (response | opCode | authoritative | truncated | recursionDesired | recursionAvailable).toByte
  	
  	val zero = header.zero << 4
  	val rCode = header.rCode
  	val zeroRCodeByte = (zero | rCode).toByte
  	
  	val questionCountBytes = int16ToBytes(header.questionCount)
  	val answerCountBytes = int16ToBytes(header.answerCount)
  	val authorityCountBytes = int16ToBytes(header.authorityCount)
  	val additionalCountBytes = int16ToBytes(header.additionalCount)
  	
  	(idBytes :+ bunchByte :+ zeroRCodeByte) ++ questionCountBytes ++ answerCountBytes ++ authorityCountBytes ++ additionalCountBytes
  }
  
}