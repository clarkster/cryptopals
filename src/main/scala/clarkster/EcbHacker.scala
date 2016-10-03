package clarkster


import scala.annotation.tailrec
import scala.util.Random

object EcbHacker {

  type Encryptor = Array[Byte] => List[Byte]

  val fixedSecretKey = Old.randomBytes(16)
  val secretText = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
  val secretBytes = Base64.decode(secretText)


  val ecbEncrpytor: Encryptor = ecbEncryptorForTextAndKey(secretBytes.bytes.toArray, fixedSecretKey)

  def ecbEncryptorForTextAndKey(secretText: Array[Byte], secretKey: Array[Byte])(plaintext: Array[Byte]): List[Byte] = {
    val input = plaintext ++ secretText
    Old.encryptEcb(input, secretKey).toList
  }

  def ecbEncryptorForTextAndKeyWithRandom(secretText: Array[Byte], secretKey: Array[Byte])(plaintext: Array[Byte]): List[Byte] = {
    val randomLength = Random.nextInt(256)
    val input = Old.randomBytes(randomLength) ++ plaintext ++ secretText
    Old.encryptEcb(input, secretKey).toList
  }

  @tailrec
  def gcd(a: Int,b: Int): Int = {
    if(b ==0) a else gcd(b, a%b)
  }

  def gcd(numbers: Seq[Int]): Int = numbers.reduce(gcd)

  def determineBlockLength(encryptor: Encryptor) : Int = {
    val blockSizes = (1 to 40).map(i => encryptor(Array.fill(i)('A'.toByte)).length)
    blockSizes.reduce(gcd)
  }


  def crackEncryptor(encryptor : Encryptor) = {
    val blockLength : Int = determineBlockLength(encryptor)
    crackRecursive(Array(), blockLength, encryptor)
  }

  def determineMarkerEncrypted(marker: Array[Byte], blockLength: Int, encryptor: Encryptor) : List[Byte] = {
    val fixedBytes = marker ++ marker ++ marker
    val encrypted = encryptor.apply(fixedBytes)
    encrypted.grouped(blockLength).map(_.toList).toSeq.groupBy(identity).maxBy(_._2.size)._1
  }

  def randomPrefixRemovingEncryptor(blockLength: Int, encryptor: Encryptor) : Encryptor = {
    val marker = Array.fill(blockLength * 3)('B'.toByte)
    val threeMarkers = List.fill(3)(determineMarkerEncrypted(marker, blockLength, encryptor))
    plaintext => {
      val streamOfEncryptions = Stream.continually(encryptor.apply(marker ++ plaintext).grouped(blockLength).map(_.toList).toList)
      val cipherBlocks : List[List[Byte]] = streamOfEncryptions.take(1000).find(cipherBlocks => cipherBlocks.indexOfSlice(threeMarkers) > 0).get
      val sizeWithoutPrefix = cipherBlocks.size - cipherBlocks.indexOfSlice(threeMarkers) - 3
      val trimmed : List[List[Byte]]  = cipherBlocks.takeRight(sizeWithoutPrefix)
      trimmed.flatten
    }
  }

  def crackEncryptorWithRandomPrefix(encryptor : Encryptor) = {
    val blockLength : Int = determineBlockLength(encryptor)
    val encryptor2 = randomPrefixRemovingEncryptor(blockLength, encryptor)
    crackRecursive(Array(), blockLength, encryptor2)
  }

  def decodeNext(decryptedSoFar: Array[Byte], blockLength: Int, encryptor: Encryptor) : Option[Byte] = {
    val fixedBytes = Array.fill((blockLength - (decryptedSoFar.length % blockLength) - 1))('A'.toByte)

    // Extract the block we are currently decrypting - use a list instead of an array so == works
    def blockOfInterest(bs: List[Byte]) : List[Byte] =
      bs.slice(decryptedSoFar.length / blockLength, fixedBytes.length + decryptedSoFar.length + 1)

    // e.g. AAAAAPlainText -> CipherText
    val testBlock  = blockOfInterest(encryptor(fixedBytes))


    (0 to 255).map(_.toByte).find { i: Byte =>
      // e.g AAAAAAPlainTextX  where X is the test byte 0..256.  This will match one of the
      val currentEncrypted = encryptor(fixedBytes ++ decryptedSoFar :+ i)
      blockOfInterest(currentEncrypted) == testBlock
    }
  }

  def crackRecursive(decryptedSoFar: Array[Byte], blockLength: Int, encryptor: Encryptor) : Array[Byte] = {
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
