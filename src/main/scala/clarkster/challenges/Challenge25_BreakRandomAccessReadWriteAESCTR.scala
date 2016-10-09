package clarkster.challenges

import clarkster._

import scala.io.Source

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
    val source = CipherText.fromBase64(Helpers.testFile(number).getLines().mkString, 16)
    val ecb = ECB(Key("YELLOW SUBMARINE"))
    val plainText = ecb.decrypt(source)

    val ctr = CTR(Key.random(16), Block.random(8))
    val cipher = ctr.encrypt(plainText)

    def edit(cipherText : CipherText, offset : Int, newText : ByteList) : CipherText = {
      val plainText = ctr.decrypt(cipherText)
      val edited = plainText.bytes.take(offset) ++ newText.bytes ++ plainText.bytes.takeRight(plainText.length - offset - newText.bytes.length)
      ctr.encrypt(edited)
    }

    val knownText = ByteList.copies(cipher.length, 0)
    val knownCipher = edit(cipher, 0, knownText) // seems too easy to replace the whole thing, am I missing something?
    val ctrStream = Helpers.xOr(knownCipher.bytes, knownText.bytes)

    val decrypted = Helpers.xOr(cipher.bytes, ctrStream)
    println("Decrypted CTR by using some known text to get the stream...")
    println(Helpers.bytesToString(decrypted))
  }
}
