package clarkster

import org.scalatest.{FlatSpec, Matchers}

class AlgorithmSpec extends FlatSpec with Matchers {

  "encryptEBC" should "decrypt to itself without padding" in {
    val data = ByteList("MELLON WUBMARINE" getBytes)
    val algorithm = ECB(Key("YELLOW SUBMARINE"), padding = NoPadding)
    val encrypted = algorithm.encrypt(data)
    val decrypted =  algorithm.decrypt(encrypted)
    data.ascii shouldBe decrypted.ascii
  }

  "encryptCBC" should "decrypt to itself with a longer string" in {
    val data : ByteList = ("MELLON WUBMARINE" * 100) getBytes
    val algorithm = CBC(Key("YELLOW SUBMARINE"), Block.copies(16, 0), padding = NoPadding)
    val encrypted = algorithm.encrypt(data)
    val decrypted =  algorithm.decrypt(encrypted)
    decrypted shouldBe data
  }
}
