package clarkster.challenges

import clarkster._

object Challenge9_ImplementPKCS7Padding extends Challenge {
  override val number: Int = 9
  override val desc: String =
    """
      |A block cipher transforms a fixed-sized block (usually 8 or 16 bytes) of plaintext into ciphertext. But we almost never want to transform a single block; we encrypt irregularly-sized messages.
      |
      |One way we account for irregularly-sized messages is by padding, creating a plaintext that is an even multiple of the blocksize. The most popular padding scheme is called PKCS#7.
      |
      |So: pad any block to a specific block length, by appending the number of bytes of padding to the end of the block. For instance,
      |
      |"YELLOW SUBMARINE"
      |... padded to 20 bytes would be:
      |
      |"YELLOW SUBMARINE\x04\x04\x04\x04"
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val padded = PKCS7.pad(20, "YELLOW SUBMARINE".bytes)
    println("Padded block is " + padded.ascii)
    assert(padded.bytes == "YELLOW SUBMARINE".getBytes.toList ++ List.fill(4)(0x04))
    println("Test successful")
  }
}
