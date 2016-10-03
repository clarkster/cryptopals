package clarkster

import java.util
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import scala.Byte._
import scala.annotation.tailrec
import scala.util.Random

object Old {

  // Scala partial functions - ref http://danielwestheide.com/blog/2013/01/30/the-neophytes-guide-to-scala-part-11-currying-and-partially-applied-functions.html
  val decryptEcb: (Array[Byte], Array[Byte]) => Array[Byte] = applyEcb(Cipher.DECRYPT_MODE)
  val encryptEcb: (Array[Byte], Array[Byte]) => Array[Byte] = applyEcb(Cipher.ENCRYPT_MODE)

  // TODO - function has a side effect, check out pure functional implementations - http://programmers.stackexchange.com/questions/202908/how-do-functional-languages-handle-random-numbers
  def randomBytes(len : Int) : Array[Byte] = Array.fill(len)(Random.nextInt(256)) map (_.toByte)
  def someRandomBytes() : Array[Byte] = randomBytes(Random.nextInt(6) + 5)

  def encryptionOracle :  (Mode, Algorithms.Encryptor) = {
    val key = Key.random(16)
    val spec = if (Random.nextBoolean()) {
      AlgorithmSpec(key, mode=ECB)
    } else {
      AlgorithmSpec(key, mode=CCB, initializationVector = Block.random(16))
    }
    val encryptor : Algorithms.Encryptor = Algorithms.encrypt(spec)
    val encryptorWithRandomPrefixSuffix : Algorithms.Encryptor =
            {plainText : ByteList => encryptor.apply(someRandomBytes ++ plainText.bytes ++ someRandomBytes)}
    (spec.mode, encryptorWithRandomPrefixSuffix)
  }

  def decryptionOracle(encryptionFunction: Algorithms.Encryptor): Mode = {
    val repeatedChars = ByteList(Array.fill(16 * 1000)('A'.toByte))
    val encrypted = encryptionFunction(repeatedChars)
    val distinct = encrypted.blocks.toSet.size
    // Our heuristic is that all unique bytes probably means CCB. ECB always encrypts the same 16 byte chunk to
    // the same bytes and its likely that the plain text would include some repetitions
    if (distinct < 5) ECB else CCB
  }

  private def applyEcb(mode: Int)(bytes: Array[Byte], key: Array[Byte]) : Array[Byte] = {
    ecbArray(cipher(mode, key), key.size)(bytes)
  }

  private def ecbArray(cipher: Cipher, blockSize: Int)(bytes: Array[Byte]) : Array[Byte] = {
    padPKCS7(bytes, blockSize) grouped(blockSize) flatMap(ecbBlock(cipher)(_)) toArray
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

  def encryptCcb(bytes: Array[Byte], key: Array[Byte], initializationVector: Array[Byte]) : Array[Byte] = {
    val c = cipher(Cipher.ENCRYPT_MODE, key)

    @tailrec
    def applyCbcRecursive(prev: Array[Byte], soFar: List[Array[Byte]], remaining: List[Array[Byte]]) : List[Array[Byte]] = {
      remaining match {
        case Nil => soFar
        case head :: tail =>
          val xor = xOr(prev, head)
          val encrypted = ecbBlock(c)(xOr(prev, head))
          //println("Head " + Hex(head) + " Prev " + Hex(prev) + " Enc " + Hex(encrypted))
          applyCbcRecursive(encrypted, encrypted :: soFar, tail)
      }
    }
    val encrypted = applyCbcRecursive(initializationVector, Nil, padPKCS7(bytes, key.size).grouped(key.size).toList)
    encrypted.reverse.flatten.toArray
  }

//  def decryptCcbOld(bytes: Array[Byte], key: Array[Byte], initializationVector: Array[Byte]) : Array[Byte] = {
//    val c = cipher(Cipher.DECRYPT_MODE, key)
//    val ecbFunction: (Array[Byte] => Array[Byte]) = ecbBlock(c)
//    val decrypted = applyCbcRecursive(ecbFunction, initializationVector, Nil, bytes.grouped(key.size).toList.reverse)
//    decrypted.reverse.flatten.toArray
//  }
  def decryptCcb(bytes: Array[Byte], key: Array[Byte], initializationVector: Array[Byte]) : Array[Byte] = {
    val c = cipher(Cipher.DECRYPT_MODE, key)
    @tailrec
    def applyCbcRecursive(prev: Array[Byte], soFar: List[Array[Byte]], remaining: List[Array[Byte]]) : List[Array[Byte]] = {
      remaining match {
        case Nil => soFar
        case head :: tail =>
          val encrypted = ecbBlock(c)(head)
          applyCbcRecursive(head, xOr(prev, encrypted) :: soFar, tail)
      }
    }
    val decrypted = applyCbcRecursive(initializationVector, Nil, bytes.grouped(key.size).toList)
    decrypted.reverse.flatten.toArray
  }


  def decodeAgainstSingleChar(bytes: ByteList) : Seq[(Byte, String, Int)] = {
    Helpers.eachByte.map { b =>
      val decode = bytes.xOrRepeating(List(b))
      val str = Helpers.bytesToString(decode.bytes)
      (b, str, Score.score(str))
    }
  }

  def bestSingleChar(msg : ByteList) : (Byte, String, Int) = {
    decodeAgainstSingleChar(msg).maxBy(_._3)
  }

  def xOr(array1: Array[Byte], array2: Array[Byte]) : Array[Byte] = {
    require(array1.length == array2.length)
    var xor = array1 zip(array2) map {
      case (b1, b2) => (b1 ^ b2) toByte
    }
    xor
  }

  // 1 iteration for each bit, 8 total. Shift right and AND 1 to get i-th bit
  // http://www.tautvidas.com/blog/2013/07/compute-hamming-distance-of-byte-arrays/
  def numberOfBitsSet(b: Byte) : Int = (0 to 7).map((i : Int) => (b >>> i) & 1).sum

  def hammingDistance(array1: Block, array2: Block) : Int = {
    require(array1.length == array2.length)

    array1.bytes.zip(array2.bytes).map{
      case (b1, b2) => numberOfBitsSet(b1 ^ b2 toByte)
    } sum
  }

  def guessHammingDistance(bytes: ByteList, keysize: Int) : Double = {
    val grouped = bytes.blocks(keysize, PKCS7) take 4
    val distances = for {s1 <- grouped
                         s2 <- grouped if (s1 != s2)}
      yield hammingDistance(s1, s2).toDouble / keysize
    distances.sum / distances.length
  }

  // Determine the likely key size used for
  def determineKeySize(bytes: ByteList) : Int = {
    val guesses = (2 to 40).map(keysize => (keysize, guessHammingDistance(bytes, keysize)))
    guesses.minBy(_._2)._1
  }

  def padPKCS7(bytes: Array[Byte], keySize: Int): Array[Byte] = {
    val pad = (keySize - bytes.length % keySize) % keySize
    val padChar: Byte = Byte.box(pad.toByte)
    bytes ++ Array.fill((keySize - bytes.length % keySize) % keySize)(padChar)
  }

}
