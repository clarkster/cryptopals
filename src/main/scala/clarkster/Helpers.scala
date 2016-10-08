package clarkster

import scala.annotation.tailrec
import scala.util.Random

object Helpers {

  def hammingDistance(array1: Block, array2: Block) : Int = {
    require(array1.length == array2.length)

    array1.bytes zip(array2.bytes) map {
      case (b1, b2) => numberOfBitsSet(b1 ^ b2 toByte)
    } sum
  }

  def numberOfBitsSet(b: Byte) : Int = (0 to 7).map(i => (b >>> i) & 1).sum

  @tailrec
  def gcd(a: Int,b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def gcd(numbers: Seq[Int]): Int = numbers.reduce(gcd)

  def eachByte() = (0 to 255) map (_.toByte)

  def bytesToString(bytes: Seq[Byte]) = bytes map(_.toChar) mkString

  def hexToBytes(hexString : String) : List[Byte] = {
    require(hexString.length % 2 == 0)
    hexString grouped (2) map (charPair => Integer.parseInt(charPair, 16) toByte) toList
  }

  def randomBytes(len : Int) : List[Byte] = List.fill(len)(Random.nextInt(256)) map (_.toByte)
  def randomLengthOfRandomBytes(minLen : Int, maxLen: Int) : List[Byte] = randomBytes(Random.nextInt(maxLen - minLen) + minLen)

  randomBytes(Random.nextInt(6) + 5)

  val hexChars = "0123456789abcdef"

  def bytesToHex(bytes: Seq[Byte]) = {
    bytes flatMap(b => Array((b & 0xf0) >> 4, b & 0xf)) map(byteToHex) mkString("")
  }

  private def byteToHex(int: Int) = hexChars.charAt(int)

  def xOr(array1: List[Byte], array2: List[Byte], truncateToShortest : Boolean = false) : List[Byte] = {
    require(truncateToShortest || array1.length == array2.length)
    array1 zip(array2) map {
      case (b1, b2) => (b1 ^ b2) toByte
    }
  }

  // Convert a Long to its big-endian byte representation. e.g. 1 -> (1, 0, 0, 0, 0, 0, 0, 0)
  def toArrayBuf(x: Long): List[Byte] = Range(0, 8).map(i => ((x >>> (i << 3)) & 0xFF).toByte).toList

  implicit def intWithTimes(n : Int) = new {
    def times(f : => Unit) = 1 to n foreach(_ => f)
  }
}
