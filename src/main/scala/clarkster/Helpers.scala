package clarkster

object Helpers {

  def eachByte() = (0 to 255) map (_.toByte)

  def bytesToString(bytes: Seq[Byte]) = bytes map(_.toChar) mkString

  def hexToBytes(hexString : String) : List[Byte] = {
    require(hexString.length % 2 == 0)
    hexString grouped (2) map (charPair => Integer.parseInt(charPair, 16) toByte) toList
  }

  val hexChars = "0123456789abcdef"

  def bytesToHex(bytes: Seq[Byte]) = {
    bytes flatMap(b => Array((b & 0xf0) >> 4, b & 0xf)) map(byteToHex) mkString("")
  }

  private def byteToHex(int: Int) = hexChars.charAt(int)

  def xOr(array1: List[Byte], array2: List[Byte]) : List[Byte] = {
    require(array1.length == array2.length)
    array1 zip(array2) map {
      case (b1, b2) => (b1 ^ b2) toByte
    }
  }
}
