package clarkster

import org.scalatest.{FlatSpec, Matchers}

class ChallengeSpec extends FlatSpec with Matchers {

  "Round 1" should "convert to base64" in {
    Challenges.round1() shouldBe "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    println("Round 1 passed")
  }

  "Round 2" should "xOR correctly" in {
    Challenges.round2() shouldBe "746865206b696420646f6e277420706c6179"
    println("Round 2 passed")
  }

  "Round 3" should "determine character for single character  Xor" in {
    val decoded = Challenges.round3()
    println("Round 3 decoded text " + decoded._2 + " using " + decoded._1.toChar)
  }

  "Round 4" should "determine string single character Xor" in {
    val decoded = Challenges.round4()
    println("Round 4 encoded string " + decoded._1 + " decodes as single byte xor to " + decoded._2)
  }

  "Round 5" should "encode repeating key Xor" in {
    Challenges.round5() shouldBe "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
    println("Round 5 passed")
  }

  "Round 6" should "decrypt repeating key Xor" in {
    println("Round 6 decrypted")
    println(Challenges.round6())
  }

  "Round 7" should "decrypt using AES" in {
    println("Round 7 decrypted")
    println(Challenges.round7())
  }

  "Round 8" should "determine AES in ECB mode line" in {
    println("Round 7 decrypted")
    println(Challenges.round8())
  }

  "Round 9" should "pad PKCS#7" in {
    println(Challenges.round9().getBytes shouldEqual "YELLOW SUBMARINE".getBytes ++ Array(0x4, 0x4, 0x4, 0x4))
  }

  "Round 10" should "Implement CBC mode" in {
    println(Challenges.round10())
  }

  "Round 11" should "Detect ECB mode" in {
    println(Challenges.round11())
  }
}
