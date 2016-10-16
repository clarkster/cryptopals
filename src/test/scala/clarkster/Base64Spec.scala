package clarkster

import org.scalatest.{FlatSpec, Matchers}
import ByteListOps._

class Base64Spec extends FlatSpec with Matchers {

  "encoder" should "pad value with one byte missing" in {
    "aa".bytes.base64 shouldBe "YWE="
  }

  it should "pad value with two bytes missing" in {
    "A".bytes.base64 shouldBe "QQ=="
  }

  it should "not pad value with no byte missing" in {
    "aaa".bytes.base64 shouldBe "YWFh"
  }

  it should "convert example" in {
    val hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d".hex
    hex.base64 shouldBe "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
  }

  "decoder" should "convert with two padding chars" in {
    "QQ==".b64.ascii shouldBe "A"
  }

  it should "convert with one padding chars" in {
    "YWE=".b64.ascii  shouldBe "aa"
  }
}
