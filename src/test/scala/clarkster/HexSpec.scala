package clarkster

import org.scalatest.{FlatSpec, Matchers}


class HexSpec extends FlatSpec with Matchers {

  "Hex String" should "convert to bytes" in {
    "49276de8".hex.bytes should be(List(73, 39, 109, -24))
  }

  "Hex String with odd length" should "fail conversion" in {
    an[IllegalArgumentException] should be thrownBy "492".hex
  }

  "Hex String" should "convert from bytes" in {
    ByteListOps(List(73.toByte, 39.toByte, 109.toByte)).hex shouldBe ("49276d")
  }
}