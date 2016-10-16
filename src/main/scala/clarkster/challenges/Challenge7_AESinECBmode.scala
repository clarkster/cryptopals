package clarkster.challenges

import clarkster._

import scala.io.Source

object Challenge7_AESinECBmode extends Challenge {
  override val number: Int = 7
  override val desc: String =
    """
      |AES in ECB mode
      |The Base64-encoded content in this file has been encrypted via AES-128 in ECB mode under the key
      |
      |"YELLOW SUBMARINE".
      |(case-sensitive, without the quotes; exactly 16 characters; I like "YELLOW SUBMARINE" because it's exactly 16 bytes long, and now you do too).
      |
      |Decrypt it. You know the key, after all.
      |
      |Easiest way: use OpenSSL::Cipher and give it AES-128-ECB as the cipher.
      |
      |Do this with code.
      |You can obviously decrypt this using the OpenSSL command-line tool, but we're having you get ECB working in code for a reason. You'll need it a lot later on, and not just for attacking ECB.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val encrypted = Helpers.testFile(number).mkString("").b64
    val decryptor = Algorithm.ECB("YELLOW SUBMARINE".key, mode = Decrypt)
    println("Decrypted Text:")
    println(decryptor(encrypted).ascii)
  }
}
