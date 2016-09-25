package clarkster

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class BytesSpec extends FlatSpec with Matchers {


  "Example String" should "xOr with another" in {
    val b1 = Hex("1c0111001f010100061a024b53535009181c")
    val b2 = Hex("686974207468652062756c6c277320657965")
    assert(b1.xOr(b2) == Hex("746865206b696420646f6e277420706c6179"))
  }

  "Ice" should "encrypt" in {
    val stanza = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    val iced = Hex(Bytes.xOrRepeating(stanza.getBytes, "ICE".getBytes))
    assert(iced.toString == "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")
  }

  "Hamming distance" should "encrypt" in {
    val val1 = "this is a test".getBytes
    val val2 = "wokka wokka!!!".getBytes
    assert(Bytes.hammingDistance(val1, val2) == 37)
  }

  "grab" should "slice" in {
    val bytes = "ABCDEF".getBytes()
    println((2 to 3).map(keysize => (keysize, bytes.slice(0, keysize).mkString("."), bytes.slice(keysize, keysize * 2).mkString("."))))
  }

  "Guess" should "be right" in {
    val encrypted = Source.fromURL(getClass.getResource("/test6.txt")).getLines().mkString("")
    Bytes.determineKeySize(Base64(encrypted).decode) shouldBe 29
  }

  "padPKCS7" should "pad 4 bytes" in {
    val bytes: Array[Byte] = Array.fill(5)(33)
    val padded = Bytes.padPKCS7(bytes, 6)
    padded.length shouldBe 6
    padded.takeRight(1) shouldBe Array(0x01)
  }

  "encryptEBC" should "decrypt to itself" in {
    val data = "MELLON WUBMARINE".getBytes
    val encrypted = Bytes.encryptEcb(data, "YELLOW SUBMARINE".getBytes)
    Bytes.decryptEcb(encrypted, "YELLOW SUBMARINE".getBytes) shouldBe data
  }

  "encryptCBC" should "decrypt to itself" in {
    val data = "MELLON WUBMARINE".getBytes
    val encrypted = Bytes.encryptCcb(data, "YELLOW SUBMARINE".getBytes, Array.fill(16)(0))
    Bytes.decryptCcb(encrypted, "YELLOW SUBMARINE".getBytes, Array.fill(16)(0)) shouldBe data
  }
}

