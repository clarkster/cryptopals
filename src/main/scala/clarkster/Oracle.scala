package clarkster

object Oracle {
  def breakRepeatingKeyXOrKey(bytes: List[Byte], keySize: Int) : List[Byte] = {
    val blocks = bytes.pad(keySize, PKCS7).grouped(keySize).toList
    val transposed = blocks.transpose

    val solveds = transposed.map(arr => Score.bestSingleChar(arr))
    solveds.map(_._1)
  }


  def detectECBOrCBC(fn: Algorithm.Encryptor) = {
    val repeatedChars = ByteListOps.copies(16 * 1000, 'A')
    val encrypted = fn(repeatedChars)
    val distinct = encrypted.blocks(16).toSet.size
    // Our heuristic is that all unique bytes probably means CCB. ECB always encrypts the same 16 byte chunk to
    // the same bytes and its likely that the plain text would include some repetitions
    if (distinct < 5) "ECB" else "CBC"
  }

  def detectECBBlockSize(bytes: List[Byte]) = {
    val guesses = (2 to 40).map(keysize => (keysize, guessHammingDistance(bytes, keysize)))
    guesses.minBy(_._2)._1
  }

  def detectECBBlockSize(encryptor: Algorithm.Encryptor) = {
    val blockSizes = (1 to 40).map(i => encryptor(ByteListOps.copies(i, 'A')).length)
    blockSizes.reduce(Helpers.gcd)
  }

  private def guessHammingDistance(bytes: List[Byte], keysize: Int) : Double = {
    val grouped = bytes.pad(keysize, PKCS7).blocks(keysize) take 4
    val distances = for {s1 <- grouped
                         s2 <- grouped if s1 != s2}
      yield s1.hammingDistance(s2).toDouble / keysize
    distances.sum / distances.length
  }


}
