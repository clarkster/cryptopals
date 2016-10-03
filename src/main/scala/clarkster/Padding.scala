package clarkster

sealed abstract class Padding {
  def pad : (Int, ByteList) => ByteList
  def unpad : ByteList => ByteList
}

case object NoPadding extends Padding {
  override def pad = (blockSize: Int, bytes : ByteList) => {
    assert(blockSize == bytes.length)
    bytes
  }

  override def unpad = (block : ByteList) => block
}

case object PKCS7 extends Padding {
  override def pad = (blockSize: Int, bytes : ByteList) => {
    val padLength = (blockSize - bytes.length % blockSize) % blockSize
    val padChar: Byte = Byte.box(padLength.toByte)
    ByteList(bytes.bytes ++ List.fill(padLength)(padChar))
  }

  override def unpad = (block : ByteList) => {
    val iterator = block.bytes.reverseIterator
    val padChar = iterator.next()
    if (padChar < block.length) {
      val otherPadChars = 1 + iterator.takeWhile(i => i == padChar).length
      if (otherPadChars != padChar) {
        throw new IllegalArgumentException("Bad padding")
      }
      ByteList(block.bytes.take(block.length - otherPadChars))
    } else {
      block
    }

  }

}