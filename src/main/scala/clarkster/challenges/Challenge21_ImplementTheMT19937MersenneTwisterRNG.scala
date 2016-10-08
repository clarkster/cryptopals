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
    val r1 = Random(100)
    println("Some random numbers...")
    5 times {
      println(r1.extract_number)
    }

    val r2 = Random(100)
    5 times r2.extract_number

    val r3 = Random(101)
    6 times r3.extract_number

    assert(r1.extract_number == r2.extract_number)
    assert(r1.extract_number != r3.extract_number)

    println("Tested same seed gives same sequence, different gives different")

  }
}