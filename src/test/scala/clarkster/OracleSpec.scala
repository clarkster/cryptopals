package clarkster

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class OracleSpec extends FlatSpec with Matchers {

  "Oracle" should "determine key size from text" in {
    val encrypted = Helpers.testFile(6).getLines().mkString("")
    Oracle.detectECBBlockSize(encrypted.b64) shouldBe 29
  }

  it should "determine block size of 16 for encryptor" in {
    val encryptor = Algorithm.ECB(rndKey(16), blockSize = 16, padding = PKCS7)
    Oracle.detectECBBlockSize(encryptor) shouldBe 16
  }
}
