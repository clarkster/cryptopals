package clarkster

import org.scalatest.{FlatSpec, Matchers}

class MD4Spec extends FlatSpec with Matchers {

  "MD4 Algorithm" should "match RFC for empty" in {
    MD4("".getBytes).hex shouldBe "31d6cfe0d16ae931b73c59d7e0c089c0"
  }

  it should "match RFC for char" in {
    MD4("a".getBytes).hex shouldBe "bde52cb31de33e46245e05fbdbd6fb24"
  }

  it should "match RFC for three chars" in {
    MD4("abc".getBytes).hex shouldBe "a448017aaf21d8525fc10ae87aa6729d"
  }

  it should "match RFC for short message" in {
    MD4("message digest".getBytes).hex shouldBe "d9130a8164549fe818874806e1c7014b"
  }

  it should "match RFC for lower alphabet" in {
    MD4("abcdefghijklmnopqrstuvwxyz".getBytes).hex shouldBe "d79e1c308aa5bbcdeea8ed63df412da9"
  }

  it should "match RFC for full alphabet" in {
    MD4("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".getBytes).hex shouldBe "043f8582f241db351ce627e153e7f0e4"
  }

  it should "match RFC for repeating numbers" in {
    MD4("12345678901234567890123456789012345678901234567890123456789012345678901234567890".getBytes).hex shouldBe "e33b4ddc9c38f2199c3e7b164fcc0536"
  }
}