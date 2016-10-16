package clarkster.challenges

import clarkster._
import clarkster.Helpers._

object Challenge23_CloneAnMT19937RNGFromItsOutput extends Challenge {
  override val number: Int = 23
  override val desc: String =
    """
      |Clone an MT19937 RNG from its output
      |The internal state of MT19937 consists of 624 32 bit integers.
      |
      |For each batch of 624 outputs, MT permutes that internal state. By permuting state regularly, MT19937 achieves a period of 2**19937, which is Big.
      |
      |Each time MT19937 is tapped, an element of its internal state is subjected to a tempering function that diffuses bits through the result.
      |
      |The tempering function is invertible; you can write an "untemper" function that takes an MT19937 output and transforms it back into the corresponding element of the MT19937 state array.
      |
      |To invert the temper transform, apply the inverse of each of the operations in the temper transform in reverse order. There are two kinds of operations in the temper transform each applied twice; one is an XOR against a right-shifted value, and the other is an XOR against a left-shifted value AND'd with a magic number. So you'll need code to invert the "right" and the "left" operation.
      |
      |Once you have "untemper" working, create a new MT19937 generator, tap it for 624 outputs, untemper each of them to recreate the state of the generator, and splice that state into a new instance of the MT19937 generator.
      |
      |The new "spliced" generator should predict the values of the original.
      |
      |Stop and think for a second.
      |How would you modify MT19937 to make this attack hard? What would happen if you subjected each tempered output to a cryptographic hash?
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val seed = RandomNumber.seed(scala.util.Random.nextInt(10000))
    val state = RandomNumber.randomDigits(seed, 624).map(i => RandomNumber.untemper(i))
    val clonedRng = RandomState(624, state)

    println("Cloned RNG. Testing the next few random numbers")

    val guessedNext = RandomNumber.randomDigits(clonedRng, 10)
    val actualNext = RandomNumber.randomDigits(seed, 634).takeRight(10)

    assert(guessedNext == actualNext)

    println("Cloned RNG successfully")

  }
}
