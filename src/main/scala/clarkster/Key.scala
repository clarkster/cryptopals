package clarkster

import javax.crypto.spec.SecretKeySpec

class Key(val bytes: List[Byte]) extends Bytes[Key] {
  def spec = new SecretKeySpec(bytes.toArray, "AES")

  override def cons(data: List[Byte]): Key = new Key(data)
}

object Key {

  def apply(bytes : ByteList, blockSize: Int, padding : Padding) : Key = {
    val b = padding.pad(blockSize, bytes)
    new Key(b.bytes)
  }

  def apply(str : String): Key = {
    apply(str.getBytes.toList, str.length, NoPadding)
  }

  def random(i: Int) : Key = {
    new Key(Old.randomBytes(16).toList)
  }

}