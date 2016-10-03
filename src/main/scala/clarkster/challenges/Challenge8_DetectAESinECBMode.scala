package clarkster.challenges

import scala.io.Source

object Challenge8_DetectAESinECBMode extends Challenge {
  override val number: Int = 8
  override val desc: String =
    """
      |Detect AES in ECB mode
      |In this file are a bunch of hex-encoded ciphertexts.
      |
      |One of them has been encrypted with ECB.
      |
      |Detect it.
      |
      |Remember that the problem with ECB is that it is stateless and deterministic; the same 16 byte plaintext block will always produce the same 16 byte ciphertext.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
      val minUniques = Source.fromURL(getClass.getResource("/test8.txt")).getLines().zipWithIndex.map(
        lineAndIndex => (lineAndIndex._2, lineAndIndex._1, lineAndIndex._1.grouped(16).toSet.size)
      ).minBy(_._3)

      println(
        s"""
          |We expect a line encrypted with ECB to have fewer unique bytes because the same 16
          |plaintext block will always produce the same 16 byte ciphertext and the original text
          |should have more repeated blocks than random characters
          |
          |The String with fewest unique groups has ${minUniques._3} distinct blocks out of ${minUniques._2.length / 16}
          |It appears on line ${minUniques._1}
          |And its value is ${minUniques._2}
        """.stripMargin)
  }
}
