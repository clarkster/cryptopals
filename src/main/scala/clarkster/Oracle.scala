package clarkster

object Oracle {
  def breakRepeatingKeyXOrKey(bytes: ByteList, keySize: Int) = {
    val blocks = bytes.blocks(keySize, PKCS7).map(_.bytes)
    val transposed = blocks.transpose

    val solveds = transposed.map(arr => Score.bestSingleChar(arr))
    Block(solveds.map(_._1))
  }


  def detectECBOrCBC(fn: Algorithms.Encryptor) = {
    val repeatedChars = ByteList.copies(16 * 1000, 'A')
    val encrypted = fn(repeatedChars)
    val distinct = encrypted.blocks.toSet.size
    // Our heuristic is that all unique bytes probably means CCB. ECB always encrypts the same 16 byte chunk to
    // the same bytes and its likely that the plain text would include some repetitions
    if (distinct < 5) ECB else CBC
  }

  def detectECBBlockSize(bytes: ByteList) = {
    val guesses = (2 to 40).map(keysize => (keysize, guessHammingDistance(bytes, keysize)))
    guesses.minBy(_._2)._1
  }

  def detectECBBlockSize(encryptor: Algorithms.Encryptor) = {
    val blockSizes = (1 to 40).map(i => encryptor(ByteList.copies(i, 'A')).length)
    blockSizes.reduce(Helpers.gcd)
  }

  private def guessHammingDistance(bytes: ByteList, keysize: Int) : Double = {
    val grouped = bytes.blocks(keysize, PKCS7) take 4
    val distances = for {s1 <- grouped
                         s2 <- grouped if (s1 != s2)}
      yield Helpers.hammingDistance(s1, s2).toDouble / keysize
    distances.sum / distances.length
  }


}
