package clarkster

object Base64 {

  def encode[T <: Bytes[T]](bytes : Bytes[T]) : String = {
    Helpers.bytesToString(bytes.bytes
      .iterator
      .sliding(3, 3).withPadding(0)
      .flatMap(threeToFour)
      .map(encode)
      .toArray
      .dropRight(padLength(bytes)) ++ Array.fill(padLength(bytes))(pad))
  }

  def decode(base64 : String) : ByteList = {
    ByteList(base64.getBytes
      .map(decode)
      .filter(_ >= 0)
      .sliding(4, 4)
      .flatMap(fourToThree)
      .take(lengthMinusPadding(base64))
      .toList)
  }

  val base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  val pad = '='.toByte

  private def padLength[T <: Bytes[T]](hex : Bytes[T]) : Int = {
    hex.length % 3 match {
      case 0 => 0
      case 1 => 2
      case 2 => 1
    }
  }

  private def lengthMinusPadding(base64 : String): Int = {
    val padLength = base64.takeRight(2).toCharArray match {
      case Array('=', '=') => 3
      case Array(_, '=') => 2
      case _=> 0
    }
    base64.length - padLength
  }

  private def encode(i: Int) : Byte =
    base64Chars charAt i toByte

  private def decode(b: Byte) : Byte =
    if (byte == pad) 0 else base64Chars indexOf b toByte


  private def threeToFour(threeBytes: Seq[Byte]) : Array[Int] = {
    // current 3 bytes as a 24 bit value. The 'lift' allows us to have missing elements for elements 1 or 2
    val group24 = (threeBytes.head << 16) | (threeBytes(1) << 8) | threeBytes(2)
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
