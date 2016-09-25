package clarkster

import org.scalatest.{FlatSpec, Matchers}


class HexSpec extends FlatSpec with Matchers {

  "Hex String" should "convert to bytes" in {
    Hex("49276de8").bytes should be(Array(73, 39, 109, -24))
  }

  "Hex String with odd length" should "fail conversion" in {
    an[IllegalArgumentException] should be thrownBy Hex("492")
  }

  "Hex String" should "convert from bytes" in {
    Hex(Array(73.toByte, 39.toByte, 109.toByte)) shouldBe Hex("49276d")
  }

  "Hex String" should "decode to ascii" in {
    Hex(Array(73.toByte, 39.toByte, 109.toByte)).toAscii should be("I'm")
  }
}