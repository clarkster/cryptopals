package clarkster

import javax.crypto.spec.SecretKeySpec

class Key(bytes: List[Byte]) extends ByteListOps(bytes) {
  def spec = new SecretKeySpec(bytes.toArray, "AES")
}

object Key {

  def apply(bytes : List[Byte], blockSize: Int, padding : Padding) : Key = {
    val b = padding.pad(blockSize, bytes)
    new Key(b)
  }

  def apply(bytes : List[Byte]): Key = {
    apply(bytes, bytes.length, NoPadding)
  }

  def apply(str : String): Key = {
    apply(str.bytes, str.length, NoPadding)
  }
}