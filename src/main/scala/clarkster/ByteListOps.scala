package clarkster

import java.nio.{ByteBuffer, ByteOrder}

class ByteListOps(val bytes:List[Byte]) {
  def length : Int = bytes.length
  def ascii : String = Raw.encode(bytes)
  def hex : String = Hex.encode(bytes)
  def base64 : String = Base64.encode(bytes)
  def key : Key = Key(bytes)

  def blocks(blockSize : Int) : List[List[Byte]] = bytes.grouped(blockSize).toList
  def pad(blockSize : Int, padding: Padding) : List[Byte]  = padding.pad(blockSize, bytes)
  def unpad(padding: Padding) : List[Byte]  = padding.unpad(bytes)
  def apply(algorithm: Algorithm.Encryptor) : List[Byte]  = algorithm.apply(bytes)
  def digest(digest : MessageSigner) : List[Byte]  = digest.sign(bytes)

  def ^(other: ByteListOps) : List[Byte]  = xOr(other)
  def xOr(other: ByteListOps) : List[Byte] =
    bytes zip other map { case (b1, b2) => (b1 ^ b2).toByte }

  def xOrRepeating(other: List[Byte] ) =
    xOr(Stream.continually(other).flatten take length toList)

  def hammingDistance(other : List[Byte] ) = {
    require(length == other.length)
    xOr(other) map {
      b => Helpers.numberOfBitsSet(b)
    } sum
  }


  def ints(order : ByteOrder) : List[Int] = {
    assert(bytes.length % 4 == 0)
    val bb = java.nio.ByteBuffer.wrap(bytes.toArray)
    bb.order(order)
    val is = bb.asIntBuffer()
    (1 to bytes.length / 4).map(_ => is.get()).toList
  }

  def bigint : BigInt = BigInt(bytes.toArray)

  override def hashCode(): Int = bytes.hashCode()
  override def equals(other: Any): Boolean =
    other match {
      case that: ByteListOps => that.bytes == bytes
      case _ => false
    }

  override def toString() : String = "ByteList: " + hex


}

object ByteListOps{

  def apply(b:Array[Byte]) = new ByteListOps(b.toList)
  def apply(b:List[Byte]) = new ByteListOps(b)
  def n(n : Int, b: Byte) = new ByteListOps(List.fill(n)(b))
  def copies(i: Int, c: Byte) : ByteListOps = n(i, c)
  def blank(size : Int) : ByteListOps = n(size, 0)

  def fromInts(byteOrder : ByteOrder)(ints: Int*) : ByteListOps = {
    val bb = java.nio.ByteBuffer.allocate(ints.size * 4)
    bb.order(byteOrder)
    bb.asIntBuffer().put(ints.toArray)
    bb.array().toList
  }

  def fromLongs(order: ByteOrder)(longs : Long*) : ByteListOps = {
    val bb = ByteBuffer.allocate(longs.size * 8)
    bb.order(order)
    bb.asLongBuffer().put(longs.toArray)
    bb.array().toList
  }
}
