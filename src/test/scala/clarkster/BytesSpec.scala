package clarkster

import java.nio.ByteOrder

import org.scalatest.{FlatSpec, Matchers}


class BytesSpec extends FlatSpec with Matchers {


  "ByteList" should "xOr with another" in {
    val b1 = "1c0111001f010100061a024b53535009181c".hex
    val b2 = "686974207468652062756c6c277320657965".hex
    assert(b1.xOr(b2).hex == "746865206b696420646f6e277420706c6179")
  }

  "ByteList" should "xOr repeating with another" in {
    val stanza = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    val iced = stanza.bytes.xOrRepeating("ICE".bytes)
    assert(iced.hex == "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")
  }

  "ByteList" should "convert Big-Endian" in {
    val stanza = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27".hex
    val iced = stanza.ints(ByteOrder.BIG_ENDIAN)
    val newList = ByteListOps.fromInts(ByteOrder.BIG_ENDIAN)(iced :_*)
    newList shouldBe stanza
  }

  "ByteList" should "convert Little-Endian" in {
    val stanza = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27".hex
    val iced = stanza.ints(ByteOrder.LITTLE_ENDIAN)
    val newList = ByteListOps.fromInts(ByteOrder.LITTLE_ENDIAN)(iced :_*)
    newList shouldBe stanza
    iced shouldNot be (stanza.ints(ByteOrder.BIG_ENDIAN))
  }
}

