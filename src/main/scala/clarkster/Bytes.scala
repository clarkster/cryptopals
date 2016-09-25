package clarkster

import java.util
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import scala.Byte._
import scala.util.Random

object Bytes {

  // Scala partial functions - ref http://danielwestheide.com/blog/2013/01/30/the-neophytes-guide-to-scala-part-11-currying-and-partially-applied-functions.html
  val decryptEcb: (Array[Byte], Array[Byte]) => Array[Byte] = applyEcb(Cipher.DECRYPT_MODE)
  val encryptEcb: (Array[Byte], Array[Byte]) => Array[Byte] = applyEcb(Cipher.ENCRYPT_MODE)
  val decryptCcb: (Array[Byte], Array[Byte], Array[Byte]) => Array[Byte] = applyCbc(Cipher.DECRYPT_MODE)
  val encryptCcb: (Array[Byte], Array[Byte], Array[Byte]) => Array[Byte] = applyCbc(Cipher.ENCRYPT_MODE)

  // TODO - function has a side effect, check out pure functional implementations - http://programmers.stackexchange.com/questions/202908/how-do-functional-languages-handle-random-numbers
  private def randomBytes(len : Int) : Array[Byte] = Array.fill(len)(Random.nextInt(256)) map (_.toByte)
  def randomAESKey() : Array[Byte] = randomBytes(16)
  def someRandomBytes() : Array[Byte] = randomBytes(Random.nextInt(6) + 5)

  def encryptionOracle(bytes: Array[Byte]): Array[Byte] = {
    val key = randomAESKey()
    val dataToEncrypt = someRandomBytes ++ bytes ++ someRandomBytes
    if (Random.nextBoolean()) { encryptEcb(dataToEncrypt, key) } else { encryptCcb(dataToEncrypt, key, randomAESKey())}
  }

  def decryptionOracle(bytes: Array[Byte]): String = {
    val numDistinct = bytes.grouped(16).toSet.size
    if (numDistinct == bytes.length / 16) "CCB" else numDistinct.toString
  }

  private def applyEcb(mode: Int)(bytes: Array[Byte], key: Array[Byte]) : Array[Byte] = {
    ecbArray(cipher(mode, key), key.size)(bytes)
  }

  private def ecbArray(cipher: Cipher, blockSize: Int)(bytes: Array[Byte]) : Array[Byte] = {
    bytes grouped(blockSize) flatMap(ecbBlock(cipher)(_)) toArray
  }

  private def ecbBlock(cipher: Cipher)(bytes: Array[Byte]) : Array[Byte] = {
    val data = cipher.doFinal(bytes)
    data
  }

  private def cipher(mode: Int, key: Array[Byte]) : Cipher = {
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(mode, new SecretKeySpec(key, "AES"))
    cipher
  }

  private def applyCbc(mode: Int)(bytes: Array[Byte], key: Array[Byte], initializationVector: Array[Byte]) : Array[Byte] = {
    val c = cipher(mode, key)
    val ecbFunction: (Array[Byte] => Array[Byte]) = ecbBlock(c)
    val bytesWithInitializationVector = initializationVector ++ bytes
    bytesWithInitializationVector.grouped(key.size).toSeq.sliding(2).flatMap {
      pair => xOr(pair(0), ecbFunction(pair(1)))
    }.toArray
  }

  def decodeAgainstSingleChar(bytes: Array[Byte]) : Seq[(Byte, String, Int)] = {
    (0 to 256).map { char =>
      val decode = Bytes.xOr(bytes, Array.fill(bytes.length)(char.toByte))
      val str = decode.map(_.toChar).mkString
      (char.toByte, str, Score.score(str))
    }
  }

  def bestSingleChar(msg : Array[Byte]) = {
    decodeAgainstSingleChar(msg).maxBy(_._3)
  }

  def xOr(array1: Array[Byte], array2: Array[Byte]) : Array[Byte] = {
    require(array1.length == array2.length)
    array1 zip(array2) map {
      case (b1, b2) => (b1 ^ b2) toByte
    }
  }

  def xOrRepeating(array1: Array[Byte], array2: Array[Byte]) : Array[Byte] = {
    return xOr(array1, Stream.continually(array2).flatten.take(array1.length) toArray)
  }

  // 1 iteration for each bit, 8 total. Shift right and AND 1 to get i-th bit
  // http://www.tautvidas.com/blog/2013/07/compute-hamming-distance-of-byte-arrays/
  def numberOfBitsSet(b: Byte) : Int = (0 to 7).map((i : Int) => (b >>> i) & 1).sum

  def hammingDistance(array1: Array[Byte], array2: Array[Byte]) : Int = {
    require(array1.length == array2.length)

    array1.zip(array2).map{
      case (b1, b2) => numberOfBitsSet(b1 ^ b2 toByte)
    } sum
  }

  def guessHammingDistance(bytes: Array[Byte], keysize: Int) : Double = {
    val grouped = bytes grouped keysize take 4 toSeq
    val distances = for {s1 <- grouped
                         s2 <- grouped if (s1 != s2)}
      yield hammingDistance(s1, s2).toDouble / keysize
    distances.sum / distances.length
  }

  // Determine the likely key size used for
  def determineKeySize(bytes: Array[Byte]) : Int = {
    val guesses = (2 to 40).map(keysize => (keysize, guessHammingDistance(bytes, keysize)))
    guesses.minBy(_._2)._1
  }

  def padPKCS7(bytes: Array[Byte], keySize: Int): Array[Byte] = {
    val pad = (keySize - bytes.length % keySize) % keySize
    val padChar: Byte = Byte.box(pad.toByte)
    bytes ++ Array.fill((keySize - bytes.length % keySize) % keySize)(padChar)
  }
}
