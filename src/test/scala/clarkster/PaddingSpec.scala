package clarkster

import org.scalatest.{FlatSpec, Matchers}

class PaddingSpec extends FlatSpec with Matchers {
  "PKC7" should "unpad valid padding" in {
    val data = PKCS7.unpad("YELLOW SUBMARI".getBytes :+ 2.toByte :+ 2.toByte)
    data.ascii shouldBe "YELLOW SUBMARI"
  }

  it should "should add padding char to text of uneven length" in {
    val bytes = ByteList.copies(5, 33)
    val padded = PKCS7.pad(6, bytes)
    padded.length shouldBe 6
    padded.bytes.takeRight(1) shouldBe List(0x01)
  }


  it should "should add full block to text of even length" in {
    val bytes = ByteList.copies(6, 33)
    val padded = PKCS7.pad(6, bytes)
    padded.length shouldBe 12
    padded.bytes.takeRight(6) shouldBe List.fill(6)(0x06)
  }

  it should "reject invalid unpadding" in {
    val data = ByteList("YELLOW SUBMARI".getBytes :+ 3.toByte :+ 2.toByte)
    an [IllegalArgumentException] should be thrownBy PKCS7.unpad(data)
  }
}
