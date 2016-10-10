package clarkster

import java.nio.{ByteBuffer, ByteOrder}

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

abstract class MessagePadding(order : ByteOrder) extends Padding {
  private def blanks(len : Int) = {
    val K : Int = (56 - len - 1) % 64
    val KPrime = if (K > 0) K else K + 64
    val blanks = List.fill(KPrime)(0.toByte)
    0x80.toByte :: blanks
  }

  private def lengthChars(len : Int) = {
    val bitLength = ByteBuffer.allocate(8)
    bitLength.order(order)
    bitLength.asLongBuffer().put(0, len * 8)
    bitLength.array().toList
  }

  def padChars(len: Int) = {
    blanks(len) ++ lengthChars(len)
  }

  override def pad = (blockSize: Int, bytes : ByteList) => {
    assert(blockSize == 64)
    bytes.bytes ++ padChars(bytes.length)
  }

  override def unpad: (ByteList) => ByteList = ???

}

case object SHA1Padding extends MessagePadding(ByteOrder.BIG_ENDIAN)
case object MD4Padding extends MessagePadding(ByteOrder.LITTLE_ENDIAN)
