package clarkster

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class OracleSpec extends FlatSpec with Matchers {

  "Oracle" should "determine key size from text" in {
    val encrypted = Source.fromURL(getClass.getResource("/test6.txt")).getLines().mkString("")
    Oracle.detectECBBlockSize(ByteList.fromBase64(encrypted)) shouldBe 29
  }

  it should "determine block size of 16 for encryptor" in {
    val encryptor : Algorithms.Encryptor = ECB(Key.random(16), blocksize = 16, padding = PKCS7).encrypt
    Oracle.detectECBBlockSize(encryptor) shouldBe 16
  }

  it should "determine block size of 32 for encryptor" in {
    val encryptor : Algorithms.Encryptor = ECB(Key.random(32), blocksize = 32, padding = PKCS7).encrypt
    Oracle.detectECBBlockSize(encryptor) shouldBe 32
  }

}
