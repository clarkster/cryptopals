package clarkster.challenges

import clarkster.Random

object Challenge22_CrackAnMT19937Seed extends Challenge {
  override val number: Int = 22
  override val desc: String =
    """
      |Crack an MT19937 seed
      |Make sure your MT19937 accepts an integer seed value. Test it (verify that you're getting the same sequence of outputs given a seed).
      |
      |Write a routine that performs the following operation:
      |
      |Wait a random number of seconds between, I don't know, 40 and 1000.
      |Seeds the RNG with the current Unix timestamp
      |Waits a random number of seconds again.
      |Returns the first 32 bit output of the RNG.
      |You get the idea. Go get coffee while it runs. Or just simulate the passage of time, although you're missing some of the fun of this exercise if you do that.
      |
      |From the 32 bit RNG output, discover the seed.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val seed = System.currentTimeMillis().toInt - (scala.util.Random.nextInt(60) + 40)
    println("Rnd seed " + seed)
    val rnd = Random(seed).extract_number

    println("Not quite sure if this is intention or not. We start guessing seeds based on the current time and work backwards until one matches")

    val testSeeds = Stream.iterate(System.currentTimeMillis().toInt)(_ - 1)

    val found = testSeeds.find(i => {
      Random(i).extract_number == rnd
    })

    assert(found.get == seed)
    println("Found seed successfully : " + found.get)
  }
}
