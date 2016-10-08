package clarkster.challenges

import clarkster._

import scala.io.Source

object Challenge20_BreakFixedNonceCTRStatistically extends Challenge {
  override val number: Int = 19
  override val desc: String =
    """
      |Break fixed-nonce CTR statistically
      |In this file find a similar set of Base64'd plaintext. Do with them exactly what you did with the first, but solve the problem differently.
      |
      |Instead of making spot guesses at to known plaintext, treat the collection of ciphertexts the same way you would repeating-key XOR.
      |
      |Obviously, CTR encryption appears different from repeated-key XOR, but with a fixed nonce they are effectively the same thing.
      |
      |To exploit this: take your collection of ciphertexts and truncate them to a common length (the length of the smallest ciphertext will work).
      |
      |Solve the resulting concatenation of ciphertexts as if for repeating- key XOR, with a key size of the length of the ciphertext you XOR'd.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val ctr = CTR(Key.random(16), Block.copies(8, 0))

    val ciphers = Source.fromURL(getClass.getResource("/test20.txt")).getLines()
      .map(ByteList.fromBase64(_))
      .map(ctr.encrypt(_))
      .toList

    val commonCipherLen = ciphers.minBy(_.length).length
    val truncated = ciphers.map(_.bytes.take(commonCipherLen)).map(Block(_))
    println("Truncated to common block length " + commonCipherLen)

    val key = Oracle.breakRepeatingKeyXOrKey(truncated, commonCipherLen)
    println("Detected CTR byte stream")

    println("Cracked text by xOr with cracked byteStream follows...")
    truncated.map(_.xOr(key)).foreach(solved =>
      println(solved.ascii)
    )}
}
