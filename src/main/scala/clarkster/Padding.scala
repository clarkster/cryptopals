package clarkster

import java.nio.ByteBuffer

sealed abstract class Padding {
  def pad : (Int, ByteList) => ByteList
  def unpad : ByteList => ByteList
}

case object NoPadding extends Padding {
  override def pad = (blockSize: Int, bytes : ByteList) => bytes
  override def unpad = (block : ByteList) => block
}

case object PKCS7 extends Padding {
  override def pad = (blockSize: Int, bytes : ByteList) => {
    val padLength = blockSize - bytes.length % blockSize
    val padChar: Byte = Byte.box(padLength.toByte)
    ByteList(bytes.bytes ++ List.fill(padLength)(padChar))
  }

  override def unpad = (block : ByteList) => {
    val iterator = block.bytes.reverseIterator
    val padChar = iterator.next()
    if (padChar >= block.length) {
      throw new IllegalArgumentException("Bad padding")
    }
    val otherPadChars = 1 + iterator.takeWhile(i => i == padChar).length
    if (otherPadChars != padChar) {
      throw new IllegalArgumentException("Bad padding")
    }
    ByteList(block.bytes.take(block.length - otherPadChars))
  }
}

case object SHA1Padding extends Padding {
  override def pad = (blockSize: Int, bytes : ByteList) => {
    assert(blockSize == 64)
    val len = bytes.length * 8 // message length in bits (always a multiple of the number of bits in a character).

    //    Pre-processing:
    //    append the bit '1' to the message e.g. by adding 0x80 if message length is a multiple of 8 bits.
    //  append 0 ≤ k < 512 bits '0', such that the resulting message length in bits
    //    is congruent to −64 ≡ 448 (mod 512)
    //  append ml, in a 64-bit big-endian integer. Thus, the total length is a multiple of 512 bits.

    val blanks = List.fill((56 - (bytes.length + 1) % 64) % 64)(0.toByte)
    val lengthAsBigEndian = ByteBuffer.allocate(8)
    lengthAsBigEndian.asLongBuffer().put(0, len)

    (bytes.bytes :+ 0x80.toByte) ::: blanks ::: lengthAsBigEndian.array().toList
  }

  override def unpad: (ByteList) => ByteList = ???
}