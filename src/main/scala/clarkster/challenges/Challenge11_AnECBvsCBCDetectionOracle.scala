package clarkster.challenges

import clarkster._
import clarkster.Algorithm._

import scala.util.Random

object Challenge11_AnECBvsCBCDetectionOracle extends Challenge {
  override val number: Int = 11
  override val desc: String =
    """
      |An ECB/CBC detection oracle
      |Now that you have ECB and CBC working:
      |
      |Write a function to generate a random AES key; that's just 16 random bytes.
      |
      |Write a function that encrypts data under an unknown key --- that is, a function that generates a random key and encrypts under it.
      |
      |The function should look like:
      |
      |encryption_oracle(your-input)
      |=> [MEANINGLESS JIBBER JABBER]
      |Under the hood, have the function append 5-10 bytes (count chosen randomly) before the plaintext and 5-10 bytes after the plaintext.
      |
      |Now, have the function choose to encrypt under ECB 1/2 the time, and under CBC the other half (just use random IVs each time for CBC). Use rand(2) to decide which to use.
      |
      |Detect the block cipher mode the function is using each time. You should end up with a piece of code that, pointed at a block box that might be encrypting ECB or CBC, tells you which one is happening.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    def encryptionOracle :  (String, Algorithm.Encryptor) = {
      val (mode : String, encryptor : Algorithm.Encryptor) = if (Random.nextBoolean()) {
        ("ECB", ECB(rndKey(16), padding = PKCS7))
      } else {
        ("CBC", CBC(rndKey(16), rnd(16), padding = PKCS7))
      }
      val encryptorWithRandomPrefixSuffix =
              {plainText : List[Byte] =>
                encryptor.apply(rnd(5, 10) ++ plainText ++ rnd(5, 10))}
      (mode, encryptorWithRandomPrefixSuffix)
    }

    100 times {
      val (mode, fn) = encryptionOracle
      val detected = Oracle.detectECBOrCBC(fn)
      assert(detected == mode)
    }
    println("Successfully detected 100 times")
  }

}
