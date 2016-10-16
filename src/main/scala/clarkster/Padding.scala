package clarkster

import java.nio.{ByteBuffer, ByteOrder}

sealed abstract class Padding {
  def pad : (Int, List[Byte]) => List[Byte]
  def unpad : List[Byte] => List[Byte]
}

case object NoPadding extends Padding {
  override def pad = (blockSize: Int, bytes : List[Byte]) => bytes
  override def unpad = (block : List[Byte]) => block
}

case object PKCS7 extends Padding {
  override def pad = (blockSize: Int, bytes : List[Byte]) => {
    val padLength = blockSize - bytes.length % blockSize
    val padChar: Byte = Byte.box(padLength.toByte)
    bytes.bytes ++ List.fill(padLength)(padChar)
  }

  override def unpad = (block : List[Byte]) => {
    val iterator = block.bytes.reverseIterator
    val padChar = iterator.next()
    if (padChar >= block.length) {
      throw new IllegalArgumentException("Bad padding")
    }
    val otherPadChars = 1 + iterator.takeWhile(i => i == padChar).length
    if (otherPadChars != padChar) {
      throw new IllegalArgumentException("Bad padding")
    }
    block.bytes.take(block.length - otherPadChars)
  }
}

abstract class MessagePadding(order : ByteOrder) extends Padding {
  private def blanks(len : Int) = {
    val K : Int = (56 - len - 1) % 64
    val KPrime = if (K > 0) K else K + 64
    val blanks = List.fill(KPrime)(0.toByte)
    0x80.toByte :: blanks
  }

  private def lengthChars(len : Int) : List[Byte] = {
    ByteListOps.fromLongs(order)(len * 8)
  }

  def padChars(len: Int) = {
    blanks(len) ++ lengthChars(len)
  }

  override def pad = (blockSize: Int, bytes : List[Byte]) => {
    assert(blockSize == 64)
    bytes.bytes ++ padChars(bytes.length)
  }

  override def unpad: (List[Byte]) => List[Byte] = ???

}

case object SHA1Padding extends MessagePadding(ByteOrder.BIG_ENDIAN)
case object MD4Padding extends MessagePadding(ByteOrder.LITTLE_ENDIAN)
