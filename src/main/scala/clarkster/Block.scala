package clarkster

class Block(val bytes: List[Byte]) extends Bytes[Block] {
  override def cons(data: List[Byte]): Block = {
    new Block(data)
  }
}


object Block {
  def random(blockSize: Int): Block = new Block(Helpers.randomBytes(blockSize))
  def apply(bytes: Array[Byte]) = new Block(bytes.toList)
  def apply(bytes: List[Byte]) = new Block(bytes)
  def copies(length: Int, byte: Int) = new Block(List.fill(length)(byte.toByte))

}
