package clarkster

class CipherText(val blocks : List[Block]) extends Bytes[CipherText] {
  override def bytes: List[Byte] = blocks.map(_.bytes).flatten

  override def cons(data: List[Byte]): CipherText = CipherText(data.grouped(16).map(Block(_)).toList)
}

object CipherText {
  def fromBase64(s: String, blockSize : Int) =
    CipherText(Base64.decode(s).bytes.grouped(blockSize).map(Block(_)).toList)

  def fromHex(s: String, blockSize : Int) =
    CipherText(Helpers.hexToBytes(s).grouped(blockSize).map(Block(_)).toList)

  def fromBytes(bytes: List[Byte], blockSize : Int) =
    CipherText(bytes.grouped(blockSize).map(Block(_)).toList)

  implicit def apply(blocks : List[Block]) : CipherText = new CipherText(blocks)
}