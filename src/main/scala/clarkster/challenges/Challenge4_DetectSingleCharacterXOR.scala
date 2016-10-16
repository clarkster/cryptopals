package clarkster.challenges

import clarkster._

import scala.io.Source

object Challenge4_DetectSingleCharacterXOR extends Challenge {
  override val number: Int = 4
  override val desc: String =
    """
      |Detect single-character XOR
      |One of the 60-character strings in this file has been encrypted by single-character XOR.
      |
      |Find it.
      |
      |(Your code from #3 should help.)
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val charNStr = Helpers.testFile(number).getLines().map(
      line => (line, Score.bestSingleChar(line.hex))
    )
    val (original, (char, msg, score)) = charNStr.maxBy(_._2._3)
    println(s"Found single byte XOR. Character $char, score $score")
    println("Original text")
    println(original)
    println("Decoded to")
    println(msg)
  }
}
