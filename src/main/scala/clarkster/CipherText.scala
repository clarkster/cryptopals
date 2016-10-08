package clarkster

class CipherText(val blocks : List[Block]) extends Bytes[CipherText] {
  override def bytes: List[Byte] = blocks.map(_.bytes).flatten

  override def cons(data: List[Byte]): CipherText = ???
}

object CipherText {
  def fromBase64(s: String, blockSize : Int) =
    CipherText(Base64.decode(s).bytes.grouped(blockSize).map(Block(_)).toList)

  implicit def apply(blocks : List[Block]) = new CipherText(blocks)
}