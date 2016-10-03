package clarkster

import org.scalatest.{FlatSpec, Matchers}

class PaddingSpec extends FlatSpec with Matchers {
  "PKC7" should "unpad valid padding" in {
    val data = PKCS7.unpad("YELLOW SUBMARI".getBytes)
    data.ascii shouldBe "YELLOW SUBMARI"
  }

  it should "pad 4 bytes" in {
    val bytes = ByteList.copies(5, 33)
    val padded = PKCS7.pad(6, bytes)
    padded.length shouldBe 6
    padded.bytes.takeRight(1) shouldBe List(0x01)
  }

  it should "reject invalid unpadding" in {
    val data = ByteList("YELLOW SUBMARI".getBytes :+ 3.toByte :+ 2.toByte)
    an [IllegalArgumentException] should be thrownBy PKCS7.unpad(data)
  }
}
