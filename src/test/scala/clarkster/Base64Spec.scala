package clarkster

import org.scalatest.{FlatSpec, Matchers}
import ByteList._

class Base64Spec extends FlatSpec with Matchers {

  "encoder" should "pad value with one byte missing" in {
    Base64.encode("aa".getBytes) shouldBe "YWE="
  }

  it should "pad value with two bytes missing" in {
    Base64.encode("A".getBytes) shouldBe "QQ=="
  }

  it should "not pad value with no byte missing" in {
    Base64.encode("aaa".getBytes) shouldBe "YWFh"
  }

  it should "convert example" in {
    val hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    ByteList.fromHex(hex).base64 should be
      "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
  }

  "decoder" should "convert with two padding chars" in {
    Base64.decode("QQ==") shouldBe ByteList("A".getBytes)
  }

  it should "convert with one padding chars" in {
    Base64.decode("YWE=")  shouldBe ByteList("aa".getBytes)
  }

  it should "convert with no padding chars" in {
    Base64.decode("YWFh")  shouldBe ByteList("aaa".getBytes)
  }
}
