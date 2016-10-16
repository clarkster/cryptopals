package clarkster.challenges

import clarkster._
import clarkster.Algorithm._

object Challenge25_BreakRandomAccessReadWriteAESCTR extends Challenge {
  override val number: Int = 25
  override val desc: String =
    """
      |Break "random access read/write" AES CTR
      |Back to CTR. Encrypt the recovered plaintext from this file (the ECB exercise) under CTR with a random key (for this exercise the key should be unknown to you, but hold on to it).
      |
      |Now, write the code that allows you to "seek" into the ciphertext, decrypt, and re-encrypt with different plaintext. Expose this as a function, like, "edit(ciphertext, key, offset, newtext)".
      |
      |Imagine the "edit" function was exposed to attackers by means of an API call that didn't reveal the key or the original plaintext; the attacker has the ciphertext and controls the offset and "new text".
      |
      |Recover the original plaintext.
      |
      |Food for thought.
      |A folkloric supposed benefit of CTR mode is the ability to easily "seek forward" into the ciphertext; to access byte N of the ciphertext, all you need to be able to do is generate byte N of the keystream. Imagine if you'd relied on that advice to, say, encrypt a disk.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val source = Helpers.testFile(number).getLines().mkString.b64
    val key = "YELLOW SUBMARINE".key

    val plainText = source.apply(ECB(key, mode=Decrypt))

    val ctr = CTR(rndKey(16), rnd(8))
    val cipher = plainText.apply(ctr)


    def edit(cipherText : List[Byte], offset : Int, newText : List[Byte]) : List[Byte] = {
      val plainText = ctr(cipherText)
      val edited = plainText.take(offset) ++ newText ++ plainText.takeRight(plainText.length - offset - newText.length)
      ctr(edited)
    }

    val knownText = ByteListOps.blank(cipher.length)
    val knownCipher = edit(cipher, 0, knownText) // seems too easy to replace the whole thing, am I missing something?
    val ctrStream = knownCipher.xOr(knownText)

    val decrypted = cipher.xOr(ctrStream)
    println("Decrypted CTR by using some known text to get the stream...")
    println(decrypted.ascii)
  }
}
