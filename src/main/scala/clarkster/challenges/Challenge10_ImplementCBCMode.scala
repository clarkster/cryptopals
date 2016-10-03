package clarkster.challenges

import clarkster._

import scala.io.Source

object Challenge10_ImplementCBCMode extends Challenge {
  override val number: Int = 10
  override val desc: String =
    """
      |Implement CBC mode
      |CBC mode is a block cipher mode that allows us to encrypt irregularly-sized messages, despite the fact that a block cipher natively only transforms individual blocks.
      |
      |In CBC mode, each ciphertext block is added to the next plaintext block before the next call to the cipher core.
      |
      |The first plaintext block, which has no associated previous ciphertext block, is added to a "fake 0th ciphertext block" called the initialization vector, or IV.
      |
      |Implement CBC mode by hand by taking the ECB function you wrote earlier, making it encrypt instead of decrypt (verify this by decrypting whatever you encrypt to test), and using your XOR function from the previous exercise to combine them.
      |
      |The file here is intelligible (somewhat) when CBC decrypted against "YELLOW SUBMARINE" with an IV of all ASCII 0 (\x00\x00\x00 &c)
      |
      |Don't cheat.
      |Do not use OpenSSL's CBC code to do CBC mode, even to verify your results. What's the point of even doing this stuff if you aren't going to learn from it?
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val source = CipherText.fromBase64(Source.fromURL(getClass.getResource("/test10.txt")).getLines().mkString, 16)
    val spec = AlgorithmSpec(key = Key("YELLOW SUBMARINE"), mode = CCB, initializationVector = Block(List.fill(16)(0)))
    val decryptor : Algorithms.Decryptor = Algorithms.decrypt(spec)
    val decrypted = decryptor.apply(source)
    println("Decrypted Poetry:")
    println(decrypted.ascii)
  }
}
