package clarkster

import java.nio.ByteBuffer

import scala.annotation.tailrec

object SHA1 {
  /*
   * Bitwise rotate a 32-bit number to the left
   */
  def leftShift(num : Int, cnt : Int) : Int = {
    (num << cnt) | (num >>> (32 - cnt))
  }

  case class SHA1State(h0: Int, h1: Int, h2 :Int, h3 :Int, h4: Int) {
    // Next state permutation
    // i is the index (0 to 79) within the block being permuted
    // w is the value of the block
    def permute(i: Int, w: Int): SHA1State = {
      val (f, k) = i match {
        case x if x <= 19 => ((h1 & h2) | ((~h1) & h3), 0x5A827999)
        case x if x <= 39 => (h1 ^ h2 ^ h3, 0x6ED9EBA1)
        case x if x <= 59 => ((h1 & h2) | (h1 & h3) | (h2 & h3), 0x8F1BBCDC)
        case _ => (h1 ^ h2 ^ h3, 0xCA62C1D6)
      }
      val temp = leftShift(h0, 5) + f + h4 + k + w
      SHA1State(temp, h0, leftShift(h1, 30), h2, h3)
    }

    def +(intermediate: SHA1State) : SHA1State =
      SHA1State(h0 + intermediate.h0, h1 + intermediate.h1, h2 + intermediate.h2, h3 + intermediate.h3, h4 + intermediate.h4)


    def bytes : List[Byte] = {
      //Produce the final hash value (big-endian) as a 160 bit number:
      val arr: Array[Int] = Array(h0, h1, h2, h3, h4)
      val bb = java.nio.ByteBuffer.allocate(20)
      bb.asIntBuffer().put(arr)
      bb.array().toList
    }
  }

  def expandToEighty(block: Block) : Vector[Int] = {
    @tailrec
    def addSuffix(soFar : Vector[Int]): Vector[Int] = {
      val i = soFar.length
      i match {
        case 80 => soFar
        case _ => addSuffix(soFar :+ leftShift(soFar(i-3) ^ soFar(i-8) ^ soFar(i-14) ^ soFar(i-16), 1))
      }
    }

    // Process the message in successive 512-bit chunks:
    // break message into 512-bit chunks
    // for each chunk
    // break chunk into sixteen 32-bit big-endian words w[i], 0 ≤ i ≤ 15
    val prefix : Vector[Int] = block.bytes.grouped(4).map(bytes => ByteBuffer.wrap(bytes.toArray).getInt).toVector

    // Extend the sixteen 32-bit words into eighty 32-bit words:
    // for i from 16 to 79
    // w[i] = (w[i-3] xor w[i-8] xor w[i-14] xor w[i-16]) leftrotate 1
    addSuffix(prefix)
  }

  def applyBlock(initialState : SHA1State, block: Block) : SHA1State = {
    expandToEighty(block)
      .zipWithIndex
      .foldLeft(initialState.copy())((state, pair) => state.permute(pair._2, pair._1))
  }

  def apply(message: ByteList, initialState: SHA1State) : SHA1State =
    message
      .blocks(64, SHA1Padding)
      .foldLeft(initialState.copy())((state, block) => state + applyBlock(state, block))

  def apply(message: ByteList) : SHA1State =
    apply(message, SHA1State(0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0))


}
