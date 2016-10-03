package clarkster

import org.scalatest.{FlatSpec, Matchers}


class HexSpec extends FlatSpec with Matchers {

  "Hex String" should "convert to bytes" in {
    Helpers.hexToBytes("49276de8") should be(Array(73, 39, 109, -24))
  }

  "Hex String with odd length" should "fail conversion" in {
    an[IllegalArgumentException] should be thrownBy Helpers.hexToBytes("492")
  }

  "Hex String" should "convert from bytes" in {
    Helpers.bytesToHex(Array(73.toByte, 39.toByte, 109.toByte)) shouldBe ("49276d")
  }
}