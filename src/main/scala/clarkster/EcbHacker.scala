package clarkster

import scala.util.Try


object EcbHacker {

  def crackEncryptor(encryptor : Algorithms.Encryptor) : Array[Byte] = {
    val blockLength : Int = Oracle.detectECBBlockSize(encryptor)
    val bytes = crackRecursive(Array(), blockLength, encryptor)
    Try(PKCS7.unpad(bytes).bytes.toArray).getOrElse(bytes)
  }

  def crackEncryptorWithRandomPrefix(encryptor : Algorithms.Encryptor) = {
    val blockLength : Int = Oracle.detectECBBlockSize(encryptor)
    val encryptor2 = randomPrefixRemovingEncryptor(blockLength, encryptor)
    crackEncryptor(encryptor2)
  }

  private def determineMarkerEncrypted(marker: Block, encryptor: Algorithms.Encryptor) : Block = {
    val fixedBytes = ByteList(marker.bytes ++ marker.bytes ++ marker.bytes)
    val encrypted = encryptor.apply(fixedBytes)
    val grouped = encrypted.blocks.groupBy(identity).maxBy(_._2.size)
    assert(grouped._2.size >= 2)
    grouped._1
  }

  def randomPrefixRemovingEncryptor(blockLength: Int, encryptor: Algorithms.Encryptor) : Algorithms.Encryptor = {
    val marker = Block.copies(blockLength * 3, 'B')
    val threeMarkers = List.fill(3)(determineMarkerEncrypted(marker, encryptor))
    plaintext => {
      val streamOfEncryptions : Stream[CipherText] = Stream.continually(encryptor.apply(marker.bytes ++ plaintext.bytes))
      val cipherBlocks : CipherText = streamOfEncryptions.take(1000).find {
        cipherBlocks => {
          cipherBlocks.blocks.indexOfSlice(threeMarkers) > 0
        }
      }.get
      val sizeWithoutPrefix = cipherBlocks.blocks.length - cipherBlocks.blocks.indexOfSlice(threeMarkers) - 3
      val trimmed : List[Block]  = cipherBlocks.blocks.takeRight(sizeWithoutPrefix)
      CipherText(trimmed)
    }
  }



  def decodeNext(decryptedSoFar: Array[Byte], blockLength: Int, encryptor: Algorithms.Encryptor) : Option[Byte] = {
    val fixedBytes = ByteList.copies(blockLength - (decryptedSoFar.length % blockLength) - 1, 'A')

    def blockOfInterest(bs: CipherText) : Block =
      bs.blocks(decryptedSoFar.length / blockLength)

    // e.g. AAAAAPlainText -> CipherText
    val testBlock  = blockOfInterest(encryptor(fixedBytes))


    (0 to 255).map(_.toByte).find { i: Byte =>
      // e.g AAAAAAPlainTextX  where X is the test byte 0..256.  This will match one of the
      val currentEncrypted = encryptor(ByteList(fixedBytes.bytes ++ decryptedSoFar :+ i))
      blockOfInterest(currentEncrypted) == testBlock
    }
  }

  private def crackRecursive(decryptedSoFar: Array[Byte], blockLength: Int, encryptor: Algorithms.Encryptor) : Array[Byte] = {
    val nextByte : Option[Byte] = decodeNext(decryptedSoFar, blockLength, encryptor)
    nextByte match {
      case None => decryptedSoFar
      case Some(x) => crackRecursive(decryptedSoFar :+ x, blockLength, encryptor)
    }
  }

//
//  // Send data: three-constant-blocks || attack || target-bytes
//  // Encrypted data: random-prefix || two-or-three-constant-blocks || block-with-some-amount-of-attack-block || remainder-of-attack-block || target-bytes
//  // the two-or-three blocks depends if the length of random-prefix is divisible by the blockLength or not
//  //
//  def decodeNext2(decryptedSoFar: Array[Byte], blockLength: Int, markerEncrypted: List[Byte], encryptor: Encryptor) : Option[Byte] = {
//    val fixedBytes = Array.fill(3 * blockLength)('A'.toByte)
//
//
//    // Extract the block we are currently decrypting
//    def setOfBlocksOfInterest(bytes: Array[Byte]) : Set[List[Byte]] = {
//      val set : scala.collection.mutable.Set[List[Byte]] =  scala.collection.mutable.Set()
//      var tries = 0
//      while (set.size < 1 && tries < 2000) {
//        // We're at the mercy of the RNG so keep going until we've seen all possible
//        val cipherBlocks = encryptor.apply(bytes).grouped(blockLength).map(_.toList).toList
//        val firstMarker = cipherBlocks
//          .zipWithIndex
//          .find(blockAndIndex => blockAndIndex._1 == markerEncrypted)
//          .map(_._2).get
//        assert(cipherBlocks(firstMarker + 1) == markerEncrypted)
//        if (cipherBlocks(firstMarker + 2) == markerEncrypted) {
//            val block = cipherBlocks(firstMarker + 3 + decryptedSoFar.length / blockLength)
//            set.add(block)
//        }
//
//        tries += 1
//      }
//      set.toSet
//    }
//
//    // e.g. AAAAAPlainText -> CipherText
//    val testBlocks  = setOfBlocksOfInterest(fixedBytes)
//
//    (0 to 255).map(_.toByte).find { i: Byte =>
//      // e.g AAAAAAPlainTextX  where X is the test byte 0..256.  This will match one of the
//      setOfBlocksOfInterest(fixedBytes ++ decryptedSoFar :+ i) == testBlocks
//    }
//  }


}
