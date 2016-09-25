package clarkster

case class Hex(val bytes : Array[Byte]) {
  override def toString() = bytes.flatMap(b => Array((b & 0xf0) >> 4, b & 0xf)).map(Hex.byteToHex).mkString("")
  def toAscii = Helpers.bytesToString(bytes)
  def xOr(other: Hex) : Hex = Hex(Bytes.xOr(bytes, other.bytes))

  override def equals(o: Any) = o match {
    case that: Hex => that.bytes.sameElements(this.bytes)
    case _ => false
  }
}

object Hex {
  val hexChars = "0123456789abcdef"

  def apply(hexString : String) : Hex = {
    require(hexString.length % 2 == 0)
    Hex(hexString sliding(2, 2) map (charPair => Integer.parseInt(charPair, 16) toByte) toArray)
  }

  private def byteToHex(int: Int) =
    hexChars.charAt(int)

  private def charToHex(char: Char) = {
    val ret = hexChars.indexOf(char)
    if (ret >= 0) ret else throw new IllegalArgumentException("Invalid " + char)
  }
}
