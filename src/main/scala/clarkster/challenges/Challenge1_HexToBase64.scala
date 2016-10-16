package clarkster.challenges

import clarkster._

object Challenge1_HexToBase64 extends Challenge {

  override val number = 1

  override val desc =
    """
      |The string:
      |
      |49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d
      |Should produce:
      |
      |SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t
      |So go ahead and make that happen. You'll need to use this code for the rest of the exercises.
      |
      |Cryptopals Rule
      |Always operate on raw bytes, never on encoded strings. Only use hex and base64 for pretty-printing.
    """.stripMargin


  override def main(args: Array[String]): Unit = {
    val plain = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d".hex

    println("hex " + plain.hex)
    println("base64 " + plain.base64)
    assert(plain.base64 == "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")

    println("Hex decoded to base64 successfully")
  }
}
