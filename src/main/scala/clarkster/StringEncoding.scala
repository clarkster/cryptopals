package clarkster

// Automatically convert Strings to Lists of Bytes,
// "PlainText".bytes => Use String dircectly as bytes
// "FF0155".hex => Interpret String as hex encoded
// "Fa3dd".b64 => Interpret as base 64 encoded
class StringEncodingOps(val str : String) extends AnyVal {
  def bytes : ByteListOps = Raw.decode(str)
  def b64 : ByteListOps = Base64.decode(str)
  def hex: ByteListOps = Hex.decode(str)
  def key: Key = Key(str)
}

trait StringEncoding {
  def encode(byteList : List[Byte]) : String
  def decode(string: String) : List[Byte]
}

object Raw extends StringEncoding {
  override def encode(byteList: List[Byte]): String = byteList map (_.toChar) mkString
  override def decode(string: String): List[Byte] = string map (_.toByte) toList
}

object Hex extends StringEncoding {
  override def decode(string: String): List[Byte] = {
    require(string.length % 2 == 0)
    string grouped 2 map (charPair => Integer.parseInt(charPair, 16) toByte) toList
  }

  val hexChars = "0123456789abcdef"
  def byteToHex(b : Byte) : String = Array((b & 0xf0) >> 4, b & 0xf) map byteToHex mkString ""
  private def byteToHex(int: Int) = hexChars.charAt(int)

  override def encode(bytes: List[Byte]): String = bytes map byteToHex mkString ""
}

object Base64 extends StringEncoding {

  def encode(bytes : List[Byte]) : String = {
    Raw.encode(bytes
      .iterator
      .sliding(3, 3).withPadding(0)
      .flatMap(threeToFour)
      .map(encode)
      .toList
      .dropRight(padLength(bytes)) ::: List.fill(padLength(bytes))(pad))
  }

  def decode(string : String) : List[Byte] = {
    Raw.decode(string)
      .map(decode)
      .filter(_ >= 0)
      .grouped(4)
      .flatMap(fourToThree)
      .take(lengthMinusPadding(string))
      .toList
  }

  val base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  val pad = '='.toByte

  private def padLength(hex : List[Byte]) : Int = {
    hex.length % 3 match {
      case 0 => 0
      case 1 => 2
      case 2 => 1
    }
  }

  private def lengthMinusPadding(base64 : Seq[Char]): Int = {
    val padLength = base64.takeRight(2) match {
      case Seq('=', '=') => 3
      case Seq(_, '=') => 2
      case _=> 0
    }
    base64.length - padLength
  }

  private def encode(i: Byte) : Byte =
    base64Chars charAt i toByte

  private def decode(b: Byte) : Byte =
    if (b == pad) 0 else base64Chars indexOf b toByte


  private def threeToFour(threeBytes: Seq[Byte]) : List[Byte] = {
    // current 3 bytes as a 24 bit value. The 'lift' allows us to have missing elements for elements 1 or 2
    val group24 = threeBytes match {
      case List(a, b, c) => (a << 16) | (b << 8) | c
    }
    List(
      (group24 >> 18) & 0x3F,
      (group24 >> 12) & 0x3F,
      (group24 >> 6) & 0x3F,
      (group24 >> 0) & 0x3F
    ) map (_.toByte)
  }

  private def fourToThree(fourBytes: List[Byte]) : List[Byte] = {
    val group24 = fourBytes match {
      case List(a, b, c, d) => a << 18 | b << 12 | c << 6 | d
    }
    List(
      (group24 >> 16) & 0xFF,
      (group24 >> 8) & 0xFF,
      (group24 >> 0) & 0xFF
    ) map (_.toByte)
  }
}
