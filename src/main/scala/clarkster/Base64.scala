package clarkster

class Base64(val bytes : Array[Byte]) {
  override def toString = Helpers.bytesToString(bytes)

  override def equals(o: Any) = o match {
    case that: Base64 => that.bytes.sameElements(this.bytes)
    case _ => false
  }

  def decode : Array[Byte] = Base64.decode(this)
}

object Base64 {
  def apply(bytes : Array[Byte]) = {
    require(bytes.length % 4 == 0)
    new Base64(bytes)
  }

  def apply(str : String) = {
    require(str.length % 4 == 0)
    new Base64(str.getBytes)
  }

  def encode(hex : Array[Byte]) : Base64 = {
    Base64(hex
      .iterator
      .sliding(3, 3).withPadding(0)
      .flatMap(threeToFour)
      .map(encode)
      .toArray
      .dropRight(padLength(hex)) ++ Array.fill(padLength(hex))(pad))
  }

  def decode(base64 : Base64) : Array[Byte] = {
    base64.bytes
      .map(decode)
      .sliding(4, 4)
      .flatMap(fourToThree)
      .take(lengthMinusPadding(base64.bytes))
      .toArray
  }

  val base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  val pad = '='.toByte

  private def padLength(hex : Array[Byte]): Int = {
    hex.length % 3 match {
      case 0 => 0
      case 1 => 2
      case 2 => 1
    }
  }

  private def lengthMinusPadding(base64 : Array[Byte]): Int = {
    val padLength = base64.takeRight(2) match {
      case Array('=', '=') => 3
      case Array(_, '=') => 2
      case _=> 0
    }
    base64.length - padLength
  }

  private def encode(int: Int) : Byte =
    base64Chars charAt(int) toByte

  private def decode(byte: Byte) : Byte =
    if (byte == pad) 0 else base64Chars indexOf (byte) toByte


  private def threeToFour(threeBytes: Seq[Byte]) : Array[Int] = {
    // current 3 bytes as a 24 bit value. The 'lift' allows us to have missing elements for elements 1 or 2
    val group24 = (threeBytes(0) << 16) | (threeBytes(1) << 8) | threeBytes(2)
    Array(
      (group24 >> 18) & 0x3F,
      (group24 >> 12) & 0x3F,
      (group24 >> 6) & 0x3F,
      (group24 >> 0) & 0x3F)
  }

  private def fourToThree(fourBytes: Array[Byte]) : Array[Byte] = {
    require(fourBytes.length == 4)
    val group24 = fourBytes(0) << 18 | fourBytes(1) << 12 | fourBytes(2) << 6 | fourBytes(3)
    Array(
      (group24 >> 16) & 0xFF,
      (group24 >> 8) & 0xFF,
      (group24 >> 0) & 0xFF
    ) map (_.toByte)
  }
}
