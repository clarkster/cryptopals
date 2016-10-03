package clarkster.challenges

import clarkster.{ByteList, Old}

object Challenge3_SingleByteXORCipher extends Challenge {
  override val number: Int = 3
  override val desc: String =
    """
      |Single-byte XOR cipher
      |The hex encoded string:
      |
      |1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
      |... has been XOR'd against a single character. Find the key, decrypt the message.
      |
      |You can do this by hand. But don't: write code to do it for you.
      |
      |How? Devise some method for "scoring" a piece of English plaintext. Character frequency is a good metric. Evaluate each output and choose the one with the best score.
      |
      |Achievement Unlocked
      |You now have our permission to make "ETAOIN SHRDLU" jokes on Twitter.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val tester = ByteList.fromHex("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

    val (char, decoded, score) = Old.bestSingleChar(tester)
    println(s"Decoded test string. Best single byte is ${char} with a score of ${score}. Decoded message is:")
    println(decoded)
  }
}
