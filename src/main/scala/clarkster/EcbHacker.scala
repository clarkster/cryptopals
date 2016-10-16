package clarkster

import scala.util.Try


object EcbHacker {

  def crackEncryptor(encryptor : Algorithm.Encryptor) : List[Byte] = {
    val blockLength : Int = Oracle.detectECBBlockSize(encryptor)
    val bytes : List[Byte] = crackRecursive(Nil, blockLength, encryptor)
    Try(PKCS7.unpad(bytes)).getOrElse(bytes)
  }

  def crackEncryptorWithRandomPrefix(encryptor : Algorithm.Encryptor) : List[Byte] = {
    val blockLength : Int = Oracle.detectECBBlockSize(encryptor)
    val encryptor2 = randomPrefixRemovingEncryptor(blockLength, encryptor)
    crackEncryptor(encryptor2)
  }

  private def determineMarkerEncrypted(marker: List[Byte], encryptor: Algorithm.Encryptor) : List[Byte] = {
    val fixedBytes = marker ++ marker ++ marker
    val encrypted = encryptor.apply(fixedBytes)
    val grouped = encrypted.grouped(16).toList.groupBy(identity).maxBy(_._2.size)
    assert(grouped._2.size >= 2)
    grouped._1
  }

  def randomPrefixRemovingEncryptor(blockLength: Int, encryptor: Algorithm.Encryptor) : Algorithm.Encryptor = {
    val marker = ByteListOps.copies(blockLength * 3, 'B')
    val threeMarkers = List.fill(3)(determineMarkerEncrypted(marker, encryptor))
    bytes => {
      val streamOfEncryptions = Stream.continually(encryptor.apply(marker.bytes ++ bytes.bytes))
      val cipherBlocks = streamOfEncryptions.take(1000).find {
        cipherBlocks => {
          cipherBlocks.grouped(16).toList.indexOfSlice(threeMarkers) > 0
        }
      }.get
      val blockStartIndex = cipherBlocks.grouped(16).toList.indexOfSlice(threeMarkers) + threeMarkers.length
      cipherBlocks.takeRight(cipherBlocks.length - blockStartIndex * blockLength)
    }
  }



  def decodeNext(decryptedSoFar: List[Byte], blockLength: Int, encryptor: Algorithm.Encryptor) : Option[Byte] = {
    val fixedBytes = ByteListOps.copies(blockLength - (decryptedSoFar.length % blockLength) - 1, 'A')

    def blockOfInterest(bs: List[Byte]) : List[Byte] =
      bs.blocks(16)(decryptedSoFar.length / blockLength)

    // e.g. AAAAAPlainText -> CipherText
    val testBlock  = blockOfInterest(encryptor(fixedBytes))


    (0 to 255).map(_.toByte).find { i: Byte =>
      // e.g AAAAAAPlainTextX  where X is the test byte 0..256.  This will match one of the
      val currentEncrypted = encryptor(ByteListOps(fixedBytes.bytes ++ decryptedSoFar :+ i))
      blockOfInterest(currentEncrypted) == testBlock
    }
  }

  private def crackRecursive(decryptedSoFar: List[Byte], blockLength: Int, encryptor: Algorithm.Encryptor) : List[Byte] = {
    val nextByte : Option[Byte] = decodeNext(decryptedSoFar, blockLength, encryptor)
    nextByte match {
      case None => decryptedSoFar
      case Some(x) => crackRecursive(decryptedSoFar :+ x, blockLength, encryptor)
    }
  }

}
