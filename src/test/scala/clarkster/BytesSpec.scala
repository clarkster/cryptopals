package clarkster

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class BytesSpec extends FlatSpec with Matchers {


  "ByteList" should "xOr with another" in {
    val b1 = ByteList.fromHex("1c0111001f010100061a024b53535009181c")
    val b2 = ByteList.fromHex("686974207468652062756c6c277320657965")
    assert(b1.xOr(b2).hex == "746865206b696420646f6e277420706c6179")
  }

  "ByteList" should "xOr repeating with another" in {
    val stanza = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    val iced = ByteList(stanza.getBytes).xOrRepeating("ICE".getBytes.toList)
    assert(iced.hex == "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")
  }

}

