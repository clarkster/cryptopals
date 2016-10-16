package clarkster

import scala.annotation.tailrec
import scala.io.Source

object Helpers {
  def leftShift(num : Int, cnt : Int) : Int = (num << cnt) | (num >>> (32 - cnt))

  def numberOfBitsSet(b: Byte) : Int = (0 to 7).map(i => (b >>> i) & 1).sum

  @tailrec
  def gcd(a: Int,b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def gcd(numbers: Seq[Int]): Int = numbers.reduce(gcd)

  def eachByte = (0 to 255) map (_.toByte)

  def xOrByte(b1 : Byte, b2 : Byte) : Byte = (b1 ^ b2).toByte

  def dump(n: Int, digits: Int = 32): String = {
    String.format("%" + digits + "s", n.toBinaryString).replace(' ', '0')
  }

  def testFile(testNo: Int) = {
    Source.fromURL(getClass.getResource("/test" + testNo + ".txt"))
  }
}
