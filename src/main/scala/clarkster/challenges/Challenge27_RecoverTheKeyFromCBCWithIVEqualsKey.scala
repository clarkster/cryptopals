package clarkster.challenges

import clarkster._
import clarkster.Algorithm._

import scala.util.{Failure, Success, Try}

object Challenge27_RecoverTheKeyFromCBCWithIVEqualsKey extends Challenge {
  override val number: Int = 27
  override val desc: String =
    """
      |Recover the key from CBC with IV=Key
      |Take your code from the CBC exercise and modify it so that it repurposes the key for CBC encryption as the IV.
      |
      |Applications sometimes use the key as an IV on the auspices that both the sender and the receiver have to know the key already, and can save some space by using it as both a key and an IV.
      |
      |Using the key as an IV is insecure; an attacker that can modify ciphertext in flight can get the receiver to decrypt a value that will reveal the key.
      |
      |The CBC code from exercise 16 encrypts a URL string. Verify each byte of the plaintext for ASCII compliance (ie, look for high-ASCII values). Noncompliant messages should raise an exception or return an error that includes the decrypted plaintext (this happens all the time in real systems, for what it's worth).
      |
      |Use your code to encrypt a message that is at least 3 blocks long:
      |
      |AES-CBC(P_1, P_2, P_3) -> C_1, C_2, C_3
      |Modify the message (you are now the attacker):
      |
      |C_1, C_2, C_3 -> C_1, 0, C_1
      |Decrypt the message (you are now the receiver) and raise the appropriate error if high-ASCII is found.
      |
      |As the attacker, recovering the plaintext from the error, extract the key:
      |
      |P'_1 XOR P'_3
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val key = "YELLOW SUBMARINE".key

    def checkProfile(profile : List[Byte]) = {
      val decrypted = profile.apply(CBC(key, key, padding=NoPadding, mode=Decrypt))
      if (decrypted.ascii.exists(c => c > 128)) {
        throw new IllegalArgumentException(decrypted.ascii)
      }
    }

    val p1p2p3 = "1234567890ABCDEF1234567890ABCDEF1234567890ABCDEF".bytes
    val c1c2c3 = p1p2p3.apply(CBC(key, key, mode=Encrypt))
    checkProfile(c1c2c3)

    val c100c1 = c1c2c3.take(16) ::: ByteListOps.blank(16).bytes ::: c1c2c3.take(16)

    val recoveredKey = Try(checkProfile(c100c1)) match {
      case Success(_) => Nil
      case Failure(e) =>
        val str = e.getMessage
        val plainBlocks = str.bytes.blocks(16)
        plainBlocks.head.xOr(plainBlocks(2))
    }
    println("Recovered key from exception message")

    assert(recoveredKey == key)
    println("And it matches")

  }
}
