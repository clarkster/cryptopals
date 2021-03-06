package clarkster.challenges

import clarkster._
import clarkster.Algorithm._

object Challenge24_CreateTheMT19937StreamCipherAndBreakIt extends Challenge {
  override val number: Int = 24
  override val desc: String =
    """
      |Create the MT19937 stream cipher and break it
      |You can create a trivial stream cipher out of any PRNG; use it to generate a sequence of 8 bit outputs and call those outputs a keystream. XOR each byte of plaintext with each successive byte of keystream.
      |
      |Write the function that does this for MT19937 using a 16-bit seed. Verify that you can encrypt and decrypt properly. This code should look similar to your CTR code.
      |
      |Use your function to encrypt a known plaintext (say, 14 consecutive 'A' characters) prefixed by a random number of random characters.
      |
      |From the ciphertext, recover the "key" (the 16 bit seed).
      |
      |Use the same idea to generate a random "password reset token" using MT19937 seeded from the current time.
      |
      |Write a function to check if any given password token is actually the product of an MT19937 PRNG seeded with the current time.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val actualSeed = scala.util.Random.nextInt(65535).toShort
    println("Init RNG with seed " + actualSeed)
    val rng = MT19937(actualSeed)

    val poetry = "Dropping MCs like a pound of bacon"
    val cipher = rng(poetry.bytes)
    assert(rng(cipher).ascii == poetry)
    println("Validated rng encryptrion")


    val algorithmToHack = Algorithm.withRandomPrepended(100, rng)

    val testTxt = ByteListOps.copies(14, 'A')
    val ciphered = algorithmToHack.apply(testTxt)
    val last14Bytes = ciphered.takeRight(14)
    val knownRandomSequence = last14Bytes.xOr(testTxt)

    println("From the test, we know that the RNG generates the following sequence somewhere: " + knownRandomSequence)

    val seeds = (0 to 65535)
      .map(_.toShort)
      .map(i => (i, RandomNumber.seed(i)))    // Bruteforce, For each seed, generated a RNG
      .map(pair => (pair._1, RandomNumber.randomDigits(pair._2, 300).map(_.toByte)))  // first n bytes since we don't know how many random bytes will be prefixed
      .filter(pair => pair._2.containsSlice(knownRandomSequence))
      .map(pair => pair._1)

    println("Found candidate RNG seeds " + seeds)
    assert(seeds.contains(actualSeed))
    println("And validated")

    def passwordResetToken() : List[Byte] = {
      val rng = RandomNumber.seed(System.currentTimeMillis().toShort)
      RandomNumber.randomDigits(rng, 20).map(_.toByte)
    }

    def isFromRng(token: List[Byte]) = {
       (0 to 65535)
        .map(_.toShort)
        .map(i => (i, RandomNumber.seed(i)))    // Bruteforce, For each seed, generated a RNG
        .map(pair => (pair._1, RandomNumber.randomDigits(pair._2, token.bytes.length)))  // first n bytes since we don't know how many random bytes will be prefixed
        .exists(pair => pair._2 == token.bytes)
     }
    val token = passwordResetToken()
    println("Generated a 'random' token")
    assert(isFromRng(token))
    println("Detected it was from a RNG")
    assert(!isFromRng(token.bytes.reverse))
    println("And not detected a non-rng token")
  }
}
