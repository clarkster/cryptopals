package clarkster

import javax.crypto.Cipher

import scala.annotation.tailrec

sealed abstract class Mode
case object ECB extends Mode
case object CCB extends Mode

case class AlgorithmSpec(
                          key : Key,
                          initializationVector: Block = Block(Nil),
                          blockLength : Int = 16,
                          padding : Padding = PKCS7,
                          mode : Mode = ECB)

object Algorithms {
  type Encryptor = ByteList => CipherText
  type Decryptor = CipherText => ByteList

  def encrypt(spec : AlgorithmSpec)(plainText : ByteList) : CipherText = {
    val blockEncryptor : Block => Block = ecbBlock(cipher(Cipher.ENCRYPT_MODE, spec.key))
    val blocks : List[Block] = plainText.blocks(spec.blockLength, spec.padding)
    val cipherBlocs = spec.mode match {
      case ECB => blocks.map(blockEncryptor)
      case CCB => encryptCcb(blocks, spec.initializationVector, blockEncryptor)
    }
    CipherText(cipherBlocs)
  }

  def decrypt(spec : AlgorithmSpec)(cipherText: CipherText) : ByteList = {
    val blockDecryptor : Block => Block = ecbBlock(cipher(Cipher.DECRYPT_MODE, spec.key))
    val blocks = spec.mode match {
      case ECB => cipherText.blocks.map(blockDecryptor)
      case CCB => decryptCcb(cipherText.blocks, spec.initializationVector, blockDecryptor)
    }
    ByteList(blocks.map(_.bytes).flatten)
  }

  private def ecbBlock(cipher: Cipher)(block: Block) : Block = {
    Block(cipher.doFinal(block.bytes.toArray).toList)
  }

  private def cipher(mode: Int, key: Key) : Cipher = {
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(mode, key.spec)
    cipher
  }

  private def encryptCcb(blocks: List[Block], initializationVector : Block, blockEncryptor: (Block) => Block) : List[Block] = {
    @tailrec
    def applyCbcRecursive(prev: Block, soFar: List[Block], remaining: List[Block]): List[Block] = {
      remaining match {
        case Nil => soFar
        case head :: tail =>
          val encrypted = blockEncryptor(prev.xOr(head))
          applyCbcRecursive(encrypted, encrypted :: soFar, tail)
      }
    }
    val encrypted = applyCbcRecursive(initializationVector, Nil, blocks)
    encrypted.reverse
  }


  private def decryptCcb(blocks: List[Block], initializationVector : Block, blockEncryptor: (Block) => Block) : List[Block] = {
    @tailrec
    def applyCbcRecursive(prev: Block, soFar: List[Block], remaining: List[Block]) : List[Block] = {
      remaining match {
        case Nil => soFar
        case head :: tail =>
          val encrypted = blockEncryptor(head)
          applyCbcRecursive(head, prev.xOr(encrypted) :: soFar, tail)
      }
    }
    val decrypted = applyCbcRecursive(initializationVector, Nil, blocks)
    decrypted.reverse
  }

}
