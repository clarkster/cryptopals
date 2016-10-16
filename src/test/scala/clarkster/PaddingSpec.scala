package clarkster

import java.nio.ByteBuffer

import org.scalatest.{FlatSpec, Matchers}

class PaddingSpec extends FlatSpec with Matchers {
  "PKC7" should "unpad valid padding" in {
    val data = PKCS7.unpad("YELLOW SUBMARI".bytes :+ 2.toByte :+ 2.toByte)
    data.ascii shouldBe "YELLOW SUBMARI"
  }

  it should "should add padding char to text of uneven length" in {
    val bytes = ByteListOps.copies(5, 33)
    val padded = PKCS7.pad(6, bytes)
    padded.length shouldBe 6
    padded.bytes.takeRight(1) shouldBe List(0x01)
  }


  it should "should add full block to text of even length" in {
    val bytes = ByteListOps.copies(6, 33)
    val padded = PKCS7.pad(6, bytes)
    padded.length shouldBe 12
    padded.bytes.takeRight(6) shouldBe List.fill(6)(0x06)
  }

  it should "reject invalid unpadding" in {
    val data = ByteListOps("YELLOW SUBMARI".bytes :+ 3.toByte :+ 2.toByte)
    an [IllegalArgumentException] should be thrownBy PKCS7.unpad(data)
  }

  "Short string" should "Sha pad to correct length" in {
    SHA1Padding.pad(64, ByteListOps.copies(55, 'A')).length shouldBe 128
  }

  "63 Byte String" should "Sha pad to correct length" in {
    SHA1Padding.pad(64, ByteListOps.copies(63, 'A')).length shouldBe 128
  }

  "63 Byte String" should "MD4 pad to correct length" in {
    MD4Padding.pad(64, ByteListOps.copies(63, 'A')).length shouldBe 128
  }


  "64 Byte String" should "Sha pad to correct length" in {
    SHA1Padding.pad(64, ByteListOps.copies(64, 'A')).length shouldBe 128
  }

  "Long string" should "Sha pad to correct length" in {
    SHA1Padding.pad(64, ByteListOps.copies(543534, 'A')).length % 64 shouldBe 0
  }

  it should "Contain original data" in {
    SHA1Padding.pad(64, ByteListOps.copies(543534, 'A')).bytes.indexOfSlice(ByteListOps.copies(543534, 'A').bytes) shouldBe 0
  }

  it should "Contain length" in {
    val padded = SHA1Padding.pad(64, ByteListOps.copies(543534, 'A'))
    val lastBytes = padded.bytes.slice(padded.length - 8, padded.length)
    ByteBuffer.wrap(lastBytes.toArray).asLongBuffer().get() shouldBe 543534 * 8
  }

  it should "Contain marker" in {
    val padded = SHA1Padding.pad(64, ByteListOps.copies(543534, 'A'))
    padded.bytes(543534) shouldBe 0x80.toByte
  }

  it should "Contain filler" in {
    val padded = SHA1Padding.pad(64, ByteListOps.copies(543534, 'A'))
    padded.bytes(543535) shouldBe 0x00
    padded.bytes(543536) shouldBe 0x00
  }

  "SHA1" should "Pad correctly" in {
    SHA1Padding.pad(64, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".bytes).length shouldBe 128
  }

  "MD4" should "Pad correctly" in {
    MD4Padding.pad(64, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".bytes).length shouldBe 128
  }
}
