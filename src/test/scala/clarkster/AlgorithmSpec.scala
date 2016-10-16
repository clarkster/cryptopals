package clarkster

import Algorithm._
import org.scalatest.{FlatSpec, Matchers}

class AlgorithmSpec extends FlatSpec with Matchers {

  val sub = "YELLOW SUBMARINE".key

  "encryptEBC" should "decrypt to itself without padding" in {
    val data = "MELLON WUBMARINE".bytes
    val encrypted = data.apply(ECB(sub, padding=NoPadding))
    val decrypted = encrypted.apply(ECB(sub, mode=Decrypt, padding = NoPadding))
    decrypted shouldBe data.bytes
  }

  "encryptCBC" should "decrypt to itself with a longer string" in {
    val data = ("MELLON WUBMARINE" * 100).bytes
    val encrypted = data.apply(CBC(sub, ByteListOps.blank(16), padding = NoPadding))
    val decrypted = encrypted.apply(CBC(sub, ByteListOps.blank(16), padding = NoPadding, mode=Decrypt))
    decrypted shouldBe data.bytes
  }

  "encryptCBC" should "decrypt to itself with an IV" in {
    val keyBytes = "YELLOW SUBMARINE".key
    val text = "1234567890ABCDEF1234567890ABCDEF1234567890ABCDEF".bytes
    val cipher = text.apply(CBC(keyBytes, keyBytes, mode=Encrypt))
    val text2 = cipher.apply(CBC(keyBytes, keyBytes, mode=Decrypt))
    text2 shouldBe text.bytes
  }

  "CTR" should "decrypt to Itself" in {
    val ctr = CTR("YELLOW SUBMARINE".key, "12345678".bytes)
    val text = "1234567890ABCDEF1234567890ABCDEF1234567890ABCDEF".bytes
    val encrypted = text.apply(ctr)
    encrypted.apply(ctr) shouldBe text.bytes
    encrypted.length shouldBe text.length
    encrypted shouldNot be (text.bytes)
  }
}
