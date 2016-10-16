package clarkster.challenges

import clarkster._
import clarkster.Helpers._

object Challenge21_ImplementTheMT19937MersenneTwisterRNG extends Challenge {
  override val number: Int = 21
  override val desc: String =
    """
      |
      |Implement the MT19937 Mersenne Twister RNG
      |You can get the psuedocode for this from Wikipedia.
      |
      |If you're writing in Python, Ruby, or (gah) PHP, your language is probably already giving you MT19937 as "rand()"; don't use rand(). Write the RNG yourself.
    """.stripMargin


  override def main(args: Array[String]): Unit = {
    val r1 = RandomNumber.seed(100)
    println("Some random numbers...")

    val seq1 = RandomNumber.randomDigits(r1, 5)
    println(seq1)
    val seq2 = RandomNumber.randomDigits(r1, 5)
    val seq3 = RandomNumber.randomDigits(RandomNumber.seed(101), 5)

    assert(seq1 == seq2)
    assert(seq1 != seq3)

    println("Tested same seed gives same sequence, different gives different")

  }
}