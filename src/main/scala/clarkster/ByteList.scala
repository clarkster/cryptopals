package clarkster


trait Bytes[T <: Bytes[T]] {
  def length : Int = bytes.length
  def xOr(text: T, truncateToShortest: Boolean = false) : T = cons(Helpers.xOr(bytes, text.bytes, truncateToShortest))
  def xOrRepeating(other: List[Byte]) = cons(Helpers.xOr(bytes, Stream.continually(other).flatten.take(length).toList))
  def ascii : String = Helpers.bytesToString(bytes)
  def hex : String = Helpers.bytesToHex(bytes)
  def base64 : String = Base64.encode(this)
  def bytes : List[Byte]
  def cons(data : List[Byte]) : T

  override def hashCode(): Int = bytes.hashCode()
  override def equals(other: Any): Boolean =
    other match {
      case that: Bytes[_] => that.bytes == bytes
      case _ => false
    }
}

// Note, we use a List[Byte] rather than an Array[Byte] so we can do equals tests (Array equality doesn't
// check the elements)
class ByteList(val bytes : List[Byte]) extends Bytes[ByteList] {
  override def cons(data : List[Byte]) : ByteList = new ByteList(data)

  def blocks(blockSize : Int, padding: Padding) : List[Block] = {
    padding.pad(blockSize, this).bytes grouped(blockSize) map(Block(_)) toList
  }
}

object ByteList {

  implicit def apply(bytes : Array[Byte]) = new ByteList(bytes.toList)
  implicit def apply(bytes : Seq[Byte]) = new ByteList(bytes.toList)
  implicit def apply(blocks : List[Block]) = new ByteList(blocks.map(_.bytes).flatten)

  def copies(length: Int, byte: Int) = new ByteList(List.fill(length)(byte.toByte))

  def fromHex(hex: String) = new ByteList(Helpers.hexToBytes(hex))
  def fromBase64(base64: String) = Base64.decode(base64)
  def fromAscii(str: String) = apply(str.getBytes)

}