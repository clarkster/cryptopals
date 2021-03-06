package clarkster.challenges

import clarkster._

object Challenge2_FixedXor extends Challenge {

  override val number = 2

  override val desc =
    """
      |Write a function that takes two equal-length buffers and produces their XOR combination.
      |
      |If your function works properly, then when you feed it the string:
      |
      |1c0111001f010100061a024b53535009181c
      |... after hex decoding, and when XOR'd against:
      |
      |686974207468652062756c6c277320657965
      |... should produce:
      |
      |746865206b696420646f6e277420706c6179
    """.stripMargin

  def main(args: Array[String]): Unit = {
    val str1 = "1c0111001f010100061a024b53535009181c".hex
    val str2 = "686974207468652062756c6c277320657965".hex
    val xOred = str1.xOr(str2)

    println(s"XOR(${str1.hex}, ${str2.hex}) = ${xOred.hex}")
    assert(xOred == "746865206b696420646f6e277420706c6179".hex)

    println("Buffers XORed successfully")
  }
}
