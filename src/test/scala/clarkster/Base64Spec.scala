package clarkster

import org.scalatest.{FlatSpec, Matchers}

class Base64Spec extends FlatSpec with Matchers {

  "encoder" should "pad value with one byte missing" in {
    Base64.encode("aa".getBytes) shouldBe Base64("YWE=")
  }

  it should "pad value with two bytes missing" in {
    Base64.encode("A".getBytes) shouldBe Base64("QQ==")
  }

  it should "not pad value with no byte missing" in {
    Base64.encode("aaa".getBytes) shouldBe Base64("YWFh")
  }

  it should "convertToString" in {
    Base64("YWFh").toString shouldBe ("YWFh")
  }

  it should "convert example" in {
    val hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    Base64.encode(Hex(hex).bytes) should be
      Base64("SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  }

  "decoder" should "convert with two padding chars" in {
    Base64.decode(Base64("QQ==")) shouldBe "A".getBytes
  }

  it should "convert with one padding chars" in {
    Base64.decode(Base64("YWE="))  shouldBe "aa".getBytes
  }

  it should "convert with no padding chars" in {
    Base64.decode(Base64("YWFh"))  shouldBe "aaa".getBytes
  }
}
