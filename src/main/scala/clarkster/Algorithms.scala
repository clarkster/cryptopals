package clarkster

import java.nio.ByteOrder
import javax.crypto.Cipher

sealed trait EncryptionMode
case object Decrypt extends EncryptionMode
case object Encrypt extends EncryptionMode

trait Algorithm {
  def apply(bytes : List[Byte]) : List[Byte]
  def fn : List[Byte] => List[Byte] = bytes => apply(bytes)
}

class ECB(key : Key,
          blockSize : Int,
          mode : EncryptionMode,
          padding : Padding) extends Algorithm {
  override def apply(bytes: List[Byte]): List[Byte] = {
    mode match {
      case Encrypt =>
        bytes.pad(blockSize, padding)
          .grouped(blockSize)
          .map(Algorithm.ecbBlock(key, mode))
          .reduce(_ ::: _)
      case Decrypt =>
        bytes
          .grouped(blockSize)
          .map(Algorithm.ecbBlock(key, mode))
          .reduce(_ ::: _)
          .unpad(padding)
    }
  }
}

class CBC(key : Key,
          iv : List[Byte],
          mode : EncryptionMode,
          padding : Padding) extends Algorithm {
  case class CBCState(prevBlock : List[Byte], coded : List[Byte], fn : List[Byte] => List[Byte])

  def encryptionFold : (CBCState, List[Byte]) => CBCState = {
    (state, block) =>
      val encrypted : List[Byte] = state.fn(state.prevBlock.xOr(block))
      CBCState(encrypted, state.coded ::: encrypted, state.fn)
  }

  def decryptionFold : (CBCState, List[Byte]) => CBCState = {
    (state, block) =>
      val decrypted = state.fn(block)
      CBCState(block, state.coded ::: state.prevBlock.xOr(decrypted).bytes, state.fn)
  }

  override def apply(bytes: List[Byte]): List[Byte] = {
    val initialState = CBCState(iv, Nil, Algorithm.ecbBlock(key, mode))
    mode match {
      case Encrypt =>
        bytes.pad(iv.length, padding)
          .grouped(iv.length)
          .foldLeft(initialState)(encryptionFold)
          .coded
      case Decrypt =>
        bytes.grouped(iv.length)
          .foldLeft(initialState)(decryptionFold)
          .coded
          .unpad(padding)
    }
  }
}

class CTR(key : Key, nonce : List[Byte])  extends Algorithm {
  override def apply(bytes: List[Byte]): List[Byte] = {
    bytes
      .grouped(16)
      .zip(ctrStream(0).iterator)
      .flatMap(pair => pair._1.xOr(pair._2))
      .toList
  }

  def ctrStream(startAt: Int) : Stream[List[Byte]] =
    Stream
      .from(startAt)
      .map(i => nonce ::: ByteListOps.fromLongs(ByteOrder.BIG_ENDIAN)(i)) // 64 bit unsigned little endian nonce, 64 bit little endian block count (byte count / 16)
      .map(Algorithm.ecbBlock(key, Encrypt))
}

class MT19937(seed: Short) extends Algorithm {
  override def apply(bytes: List[Byte]): List[Byte] = {
    bytes
      .zip(mt19937Stream)
      .map(pair => Helpers.xOrByte(pair._1, pair._2))
  }

  private def mt19937Stream : Stream[Byte] = {
    val rng = RandomNumber.seed(seed)
    RandomNumber.seq(rng).map(_._1.toByte)
  }
}

object Algorithm {
  type Encryptor = (List[Byte] => List[Byte])

  def ecbBlock(key: Key, mode: EncryptionMode): List[Byte] => List[Byte] = {
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(mode match {
      case Encrypt => Cipher.ENCRYPT_MODE
      case Decrypt => Cipher.DECRYPT_MODE
    }, key.spec)
    block => cipher doFinal block.bytes.toArray toList
  }


  def ECB(key : Key,
          blockSize : Int = 16,
          mode : EncryptionMode = Encrypt,
          padding : Padding = PKCS7) : Encryptor = new ECB(key, blockSize, mode, padding).fn


  def CBC(key : Key,
          iv : List[Byte],
          mode : EncryptionMode = Encrypt,
          padding : Padding = PKCS7) : Encryptor = new CBC(key, iv, mode, padding).fn

  def MT19937(seed : Short) : Encryptor = new MT19937(seed).fn

  def CTR(key : Key, nonce : List[Byte]) : Encryptor = new CTR(key, nonce).fn

  def transformingPlaintext(encryptor: Encryptor, transformer: List[Byte] => List[Byte]) : Encryptor =
    bytes => encryptor.apply(transformer.apply(bytes))

  def withRandomPrepended(maxLen : Int, encryptor: Encryptor) : Encryptor =
    transformingPlaintext(encryptor, byteList => rnd(0, 256) ++ byteList.bytes)

  def withSecretAppended(secret: List[Byte], encryptor: Encryptor) : Encryptor =
    transformingPlaintext(encryptor, byteList => byteList.bytes ++ secret.bytes)

}
