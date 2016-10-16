package clarkster

import java.nio.ByteOrder

import scala.annotation.tailrec

trait MessageSigner {
  def sign(message : List[Byte]) : List[Byte]
  def verify(message : List[Byte], signature : List[Byte]) : Boolean = sign(message) == signature
}

abstract class MessageDigest[T <: MessageDigest[T]] extends MessageSigner {
  def bytes : List[Byte]

  def update(block: List[Byte]): T = {
    this + permuteBlock(block)
  }

  protected def permuteBlock(block: List[Byte]): T
  protected def +(other : T): T
  protected def paddingMode : Padding

  override def sign(message : List[Byte]) : List[Byte] =
    message
      .pad(64, paddingMode)
      .blocks(64)
      .foldLeft(this)((state, block) => state.update(block))
      .bytes
}


case class SHA(h0: Int, h1: Int, h2 :Int, h3 :Int, h4: Int) extends MessageDigest[SHA] {
  protected def +(other: SHA) : SHA =
    SHA(h0 + other.h0, h1 + other.h1, h2 + other.h2, h3 + other.h3, h4 + other.h4)

  override def bytes : List[Byte] = {
    ByteListOps.fromInts(ByteOrder.BIG_ENDIAN)(h0, h1, h2, h3, h4)
  }


  // Next state permutation
  // i is the index (0 to 79) within the block being permuted
  // w is the value of the block
  private def permute(i: Int, w: Int): SHA = {
    val (f, k) = i match {
      case x if x <= 19 => ((h1 & h2) | ((~h1) & h3), 0x5A827999)
      case x if x <= 39 => (h1 ^ h2 ^ h3, 0x6ED9EBA1)
      case x if x <= 59 => ((h1 & h2) | (h1 & h3) | (h2 & h3), 0x8F1BBCDC)
      case _ => (h1 ^ h2 ^ h3, 0xCA62C1D6)
    }
    SHA(Helpers.leftShift(h0, 5) + f + h4 + k + w, h0, Helpers.leftShift(h1, 30), h2, h3)
  }

  protected def permuteBlock(block: List[Byte]) : SHA = {
    assert(block.length == 64)

    expandToEighty(block)
      .zipWithIndex
      .foldLeft(this)((state, pair) => state.permute(pair._2, pair._1))
  }

  private def expandToEighty(block: List[Byte]) : Vector[Int] = {
    @tailrec
    def addSuffix(soFar : Vector[Int]): Vector[Int] = {
      val i = soFar.length
      i match {
        case 80 => soFar
        case _ => addSuffix(soFar :+ Helpers.leftShift(soFar(i-3) ^ soFar(i-8) ^ soFar(i-14) ^ soFar(i-16), 1))
      }
    }

    // Process the message in successive 512-bit chunks:
    // break message into 512-bit chunks
    // for each chunk
    // break chunk into sixteen 32-bit big-endian words w[i], 0 ≤ i ≤ 15
    val prefix : Vector[Int] = block.ints(ByteOrder.BIG_ENDIAN).toVector

    // Extend the sixteen 32-bit words into eighty 32-bit words:
    // for i from 16 to 79
    // w[i] = (w[i-3] xor w[i-8] xor w[i-14] xor w[i-16]) leftrotate 1
    addSuffix(prefix)
  }

  override protected def paddingMode: Padding = SHA1Padding
}

object SHA extends MessageSigner {
  def default : SHA = new SHA(0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0)

  def apply(message : List[Byte]) : List[Byte] = {
    default.sign(message)
  }

  def withKey(key: Key) : MessageSigner = new MessageSigner {
    override def sign(message: List[Byte]): List[Byte] = {
      default.sign(key.bytes ++ message.bytes)
    }
  }

  override def sign(message: List[Byte]): List[Byte] = {
    default.sign(message)
  }

  def fromSig(bytes : List[Byte]) : SHA = {
    assert(bytes.size == 20)
    val List(h0, h1, h2, h3, h4) = bytes.ints(ByteOrder.BIG_ENDIAN)
    SHA(h0, h1, h2, h3, h4)
  }
}

trait MD4POS
case object A extends MD4POS
case object B extends MD4POS
case object C extends MD4POS
case object D extends MD4POS

case class MD4(a : Int, b : Int, c : Int, d : Int) extends MessageDigest[MD4] {
  override protected def paddingMode: Padding = MD4Padding

  def get(pos : MD4POS) : Int = pos match {
    case A => a
    case B => b
    case C => c
    case D => d
  }

  def newState(pos : MD4POS, newVal : Int) = pos match {
    case A => MD4(newVal, b, c, d)
    case B => MD4(a, newVal, c, d)
    case C => MD4(a, b, newVal, d)
    case D => MD4(a, b, c, newVal)
  }

  def addBytes(x: List[Int]): MD4 = {

    // The list of transforms, as taken directly from the RFC
    val transforms : List[(MD4 => MD4)] = List (
      MD4.transform1(A, B, C, D, x(0), 3),
      MD4.transform1(D, A, B, C, x(1), 7),
      MD4.transform1(C, D, A, B, x(2), 11),
      MD4.transform1(B, C, D, A, x(3), 19),
      MD4.transform1(A, B, C, D, x(4), 3),
      MD4.transform1(D, A, B, C, x(5), 7),
      MD4.transform1(C, D, A, B, x(6), 11),
      MD4.transform1(B, C, D, A, x(7), 19),
      MD4.transform1(A, B, C, D, x(8), 3),
      MD4.transform1(D, A, B, C, x(9), 7),
      MD4.transform1(C, D, A, B, x(10), 11),
      MD4.transform1(B, C, D, A, x(11), 19),
      MD4.transform1(A, B, C, D, x(12), 3),
      MD4.transform1(D, A, B, C, x(13), 7),
      MD4.transform1(C, D, A, B, x(14), 11),
      MD4.transform1(B, C, D, A, x(15), 19),

      MD4.transform2(A, B, C, D, x(0), 3),
      MD4.transform2(D, A, B, C, x(4), 5),
      MD4.transform2(C, D, A, B, x(8), 9),
      MD4.transform2(B, C, D, A, x(12), 13),
      MD4.transform2(A, B, C, D, x(1), 3),
      MD4.transform2(D, A, B, C, x(5), 5),
      MD4.transform2(C, D, A, B, x(9), 9),
      MD4.transform2(B, C, D, A, x(13), 13),
      MD4.transform2(A, B, C, D, x(2), 3),
      MD4.transform2(D, A, B, C, x(6), 5),
      MD4.transform2(C, D, A, B, x(10), 9),
      MD4.transform2(B, C, D, A, x(14), 13),
      MD4.transform2(A, B, C, D, x(3), 3),
      MD4.transform2(D, A, B, C, x(7), 5),
      MD4.transform2(C, D, A, B, x(11), 9),
      MD4.transform2(B, C, D, A, x(15), 13),

      MD4.transform3(A, B, C, D, x(0), 3),
      MD4.transform3(D, A, B, C, x(8), 9),
      MD4.transform3(C, D, A, B, x(4), 11),
      MD4.transform3(B, C, D, A, x(12), 15),
      MD4.transform3(A, B, C, D, x(2), 3),
      MD4.transform3(D, A, B, C, x(10), 9),
      MD4.transform3(C, D, A, B, x(6), 11),
      MD4.transform3(B, C, D, A, x(14), 15),
      MD4.transform3(A, B, C, D, x(1), 3),
      MD4.transform3(D, A, B, C, x(9), 9),
      MD4.transform3(C, D, A, B, x(5), 11),
      MD4.transform3(B, C, D, A, x(13), 15),
      MD4.transform3(A, B, C, D, x(3), 3),
      MD4.transform3(D, A, B, C, x(11), 9),
      MD4.transform3(C, D, A, B, x(7), 11),
      MD4.transform3(B, C, D, A, x(15), 15)
    )
    transforms.foldLeft(this)((state, transform) => transform.apply(state))
  }

  override def permuteBlock(block: List[Byte]): MD4 = {
    assert(block.length == 64)
    val is = block.ints(ByteOrder.LITTLE_ENDIAN)
    addBytes(is)
  }

  override def bytes: List[Byte] = {
    ByteListOps.fromInts(ByteOrder.LITTLE_ENDIAN)(a, b, c, d)
  }

  override protected def +(other: MD4): MD4 = {
    MD4(a + other.a, b + other.b, c + other.c, d + other.d)
  }
}



object MD4 extends MessageSigner {
  // Functions F, G, H from the RFC
  def f = (x : Int, y : Int, z : Int) => (x & y) | (~x & z)
  def g = (x : Int, y : Int, z : Int) => (x & y) | (x & z) | (y & z)
  def h = (x : Int, y : Int, z : Int) => x ^ y ^ z

  // The three Transformations from the RFC
  def transform1(pos : MD4POS, arg1: MD4POS, arg2 : MD4POS, arg3 : MD4POS, x : Int, s: Int) : (MD4 => MD4) = state =>
    state.newState(pos, Helpers.leftShift(state.get(pos) + f(state.get(arg1), state.get(arg2), state.get(arg3)) + x, s))

  def transform2(pos : MD4POS, arg1: MD4POS, arg2 : MD4POS, arg3 : MD4POS, x : Int, s: Int) : (MD4 => MD4) = state =>
    //A = (A + g(B,C,D) + X[i] + 5A827999) <<< s
    state.newState(pos, Helpers.leftShift(state.get(pos) + g(state.get(arg1), state.get(arg2), state.get(arg3)) + x + 0x5A827999, s))

  def transform3(pos : MD4POS, arg1: MD4POS, arg2 : MD4POS, arg3 : MD4POS, x : Int, s: Int) : (MD4 => MD4) = state =>
    // A = (A + h(B,C,D) + X[i] + 6ED9EBA1) <<< s
    state.newState(pos, Helpers.leftShift(state.get(pos) + h(state.get(arg1), state.get(arg2), state.get(arg3)) + x + 0x6ED9EBA1, s))


  def default : MD4 = new MD4(0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476)

  def apply(message : List[Byte]) : List[Byte] = {
    default.sign(message)
  }

  def withKey(key: Key) : MessageSigner = new MessageSigner {
    override def sign(message: List[Byte]): List[Byte] = {
      default.sign(key.bytes ++ message.bytes)
    }
  }

  override def sign(message: List[Byte]): List[Byte] = {
    default.sign(message)
  }

  def fromSig(bytes : List[Byte]) : MD4 = {
    assert(bytes.size == 16)
    val List(a, b, c, d) = bytes.ints(ByteOrder.LITTLE_ENDIAN)
    MD4(a, b, c, d)
  }
}
