package clarkster.challenges

import clarkster.{ByteList, Helpers, PKCS7}

import scala.util.{Failure, Success, Try}

object Challenge15_PKCS7PaddingValidation extends Challenge {
  override val number: Int = 15
  override val desc: String =
    """
      |PKCS#7 padding validation
      |Write a function that takes a plaintext, determines if it has valid PKCS#7 padding, and strips the padding off.
      |
      |The string:
      |
      |"ICE ICE BABY\x04\x04\x04\x04"
      |... has valid padding, and produces the result "ICE ICE BABY".
      |
      |The string:
      |
      |"ICE ICE BABY\x05\x05\x05\x05"
      |... does not have valid padding, nor does:
      |
      |"ICE ICE BABY\x01\x02\x03\x04"
      |If you are writing in a language with exceptions, like Python or Ruby, make your function throw an exception on bad padding.
      |
      |Crypto nerds know where we're going with this. Bear with us.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    assert(PKCS7.unpad(ByteList.fromAscii("ICE ICE BABY").bytes ++ ByteList.copies(4, 4).bytes).ascii == "ICE ICE BABY")
    println("Valid padding unpadded succesfully")

    val unpadded = Try(PKCS7.unpad("ICE ICE BABY".getBytes ++ Array.fill(4)(5.toByte)))

    assert(unpadded match {
      case Success(_) => false
      case Failure(_) => true
    })

    println("Invalid padding detected and exception thrown")
  }

}