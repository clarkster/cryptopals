package clarkster

import javax.crypto.Cipher

import scala.annotation.tailrec

sealed abstract class Algorithm

case object ECB extends Algorithm {
  def apply(key : Key, blocksize : Int = 16, padding : Padding = PKCS7) : SymmetricalBlockEncryptor = {
    new ECBAlgorithm(key, blocksize, padding)
  }
}

case object CBC extends Algorithm {
  def apply(key : Key, initializationVector : Block, blocksize : Int = 16, padding : Padding = PKCS7) : SymmetricalBlockEncryptor = {
    new CBCAlgorithm(key, initializationVector, blocksize, padding)
  }
}

case object CTR extends Algorithm {
  def apply(key : Key, nonce : Block) : SymmetricalBlockEncryptor = {
    assert(nonce.length == 8)
    new CTRAlgorithm(key, nonce)
  }
}

case object MT19937 extends Algorithm {
  def apply(seed: Short) : SymmetricalBlockEncryptor = {
    new MT19937Algorithm(seed)
  }
}

sealed abstract class SymmetricalBlockEncryptor {
  def encrypt(plainText: ByteList) : CipherText
  def decrypt(cipherText: CipherText) : ByteList

  protected def blockFunction(key: Key, mode: Int) : Block => Block = {
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(mode, key.spec)
    block : Block => Block(cipher.doFinal(block.bytes.toArray))
  }
}

class ECBAlgorithm(key : Key, blockSize: Int, padding: Padding) extends SymmetricalBlockEncryptor  {
  override def encrypt(plainText: ByteList): CipherText = {
    plainText.blocks(blockSize, padding).map(blockFunction(key, Cipher.ENCRYPT_MODE))
  }

  override def decrypt(cipherText: CipherText): ByteList = {
    cipherText.blocks.map(blockFunction(key, Cipher.DECRYPT_MODE))
  }
}

class CBCAlgorithm(key : Key, initializationVector: Block, blockSize: Int, padding: Padding) extends SymmetricalBlockEncryptor {
  override def encrypt(plainText: ByteList): CipherText = {
    encryptCcb(plainText.blocks(blockSize, padding), initializationVector, blockFunction(key, Cipher.ENCRYPT_MODE))
  }

  override def decrypt(cipherText: CipherText): ByteList = {
    decryptCcb(cipherText.blocks, initializationVector, blockFunction(key, Cipher.DECRYPT_MODE))
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

class CTRAlgorithm(key : Key, nonce : Block) extends SymmetricalBlockEncryptor {
  override def encrypt(plainText: ByteList): CipherText =
    ctr(plainText.blocks(16, NoPadding))

  override def decrypt(cipherText: CipherText): ByteList =
    ctr(cipherText.blocks)

  private def ctr(blocks: List[Block]) : List[Block] =
    blocks
      .zip(ctrStream(0))
      .map(pair => pair._1.xOr(pair._2, truncateToShortest = true))


  private def ctrStream(startAt: Int) : Stream[Block] =
    Stream
      .from(0)
      .map(i => Block(nonce.bytes ::: Helpers.toArrayBuf(i))) // 64 bit unsigned little endian nonce, 64 bit little endian block count (byte count / 16)
      .map(blockFunction(key, Cipher.ENCRYPT_MODE))

}

class MT19937Algorithm(seed : Short) extends SymmetricalBlockEncryptor {
  override def encrypt(plainText: ByteList): CipherText =
    CipherText(mt19937(plainText.bytes))

  override def decrypt(cipherText: CipherText): ByteList =
    mt19937(cipherText.bytes)

  private def mt19937(bytes: List[Byte]) : List[Block] =
    bytes
      .zip(mt19937Stream)
      .map(pair => Block(List(Helpers.xOrByte(pair._1, pair._2))))


  private def mt19937Stream : Stream[Byte] = {
    val rng = Random(seed)
    Stream
      .continually(rng.extract_number.toByte)
  }
}


object Algorithms {

  type Encryptor = ByteList => CipherText
  type Decryptor = CipherText => ByteList

  def transformingPlaintext(encryptor: Encryptor, transformer: ByteList => ByteList) : Encryptor = {
    byteList: ByteList => {
      encryptor.apply(transformer.apply(byteList))
    }
  }

  def withRandomPrepended(maxLen : Int, encryptor: Encryptor) : Encryptor = {
    transformingPlaintext(encryptor, byteList => Helpers.randomLengthOfRandomBytes(0, 256) ++ byteList.bytes)
  }

  def withSecretAppended(secret: ByteList, encryptor: Encryptor) : Encryptor = {
    transformingPlaintext(encryptor, byteList => byteList.bytes ++ secret.bytes)
  }

}
