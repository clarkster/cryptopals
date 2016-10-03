package clarkster

class Block(val bytes: List[Byte]) extends Bytes[Block] {
  override def cons(data: List[Byte]): Block = {
    new Block(data)
  }
}


object Block {
  def random(blockSize: Int): Block = new Block(Old.randomBytes(blockSize).toList)

  def apply(bytes: List[Byte]) = new Block(bytes)
}
