package clarkster

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import scala.io.Source

object Challenges {

  def round1() = {
    Base64.encode(
      Hex("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d").bytes
    ).toString
  }

  def round2() = {
    Hex("1c0111001f010100061a024b53535009181c").xOr(Hex("686974207468652062756c6c277320657965")).toString()
  }

  def round3() = {
    Bytes.bestSingleChar(Hex("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736").bytes)
  }

  def round4() = {
    val charNStr = Source.fromURL(getClass.getResource("/test4.txt")).getLines().map(
      line => (line, Bytes.bestSingleChar(Hex(line).bytes))
    )
    val max = charNStr.maxBy(_._2._3)
    (max._1, max._2._2)
  }

  def round5() = {
    val stanza = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    Hex(Bytes.xOrRepeating(stanza.getBytes, "ICE".getBytes)).toString
  }

  def round6() = {
    val bytes = Base64(Source.fromURL(getClass.getResource("/test6.txt")).getLines().mkString("")).decode
    val keySize = Bytes.determineKeySize(bytes)
    val blocks = bytes.grouped(keySize) toArray
    val transposed = blocks.transpose toSeq

    val solveds = transposed.map(arr => Bytes.bestSingleChar(arr))
    val key = solveds.map(_._1).toArray

    val decoded = Bytes.xOrRepeating(bytes, key)
    decoded.map(_.toChar).mkString
  }

  def round7() = {
    val encrypted = Source.fromURL(getClass.getResource("/test7.txt")).getLines().mkString("")
    Helpers.bytesToString(Bytes.decryptEcb(Base64(encrypted).decode, "YELLOW SUBMARINE".getBytes()))
  }


  def round8() = {
    // unique 16 bytes. We expect a line encrypted with ECB to have fewer unique bytes because the same 16 byte
    // plaintext block will always produce the same 16 byte ciphertext and the original text should have more repeated
    // blocks than random characters
    val minUniques = Source.fromURL(getClass.getResource("/test8.txt")).getLines().map(
      line => (line, line.grouped(16).toSet.size)
    ).minBy(_._2)
    minUniques._1
  }


  def round9() = {
    Helpers.bytesToString(Bytes.padPKCS7("YELLOW SUBMARINE".getBytes, 20))
  }

  def round10() = {
    val source = Base64(Source.fromURL(getClass.getResource("/test10.txt")).getLines().mkString(""))
    Helpers.bytesToString(Bytes.decryptCcb(source.decode, "YELLOW SUBMARINE".getBytes, Array.fill(16)(0)))
  }

  def round11() = {
    val stanza = "MELLON WUBMARINE" * (100)
    val encrypted: List[Array[Byte]] = (1 to 100).map(_ => Bytes.encryptionOracle(stanza.getBytes())) toList
    var l = (for {arr : Array[Byte] <- encrypted}
      yield Bytes.decryptionOracle(arr))
    l.mkString("\n")
  }

}
