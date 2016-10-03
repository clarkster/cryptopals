package clarkster

import org.scalatest.{FlatSpec, Matchers}

class EcbHackerSpec extends FlatSpec with Matchers {

  val bytes : Array[Byte] = Array(5, 9, 34, 23, 45, 65, 2, 3, 6, 7, 8, 9, 34, 12, 24, 45, 67, 87, 54).map(_.toByte)
  val k = Old.randomBytes(8)


  "Hacker" should "determine block size of 16" in {
    val encryptor : EcbHacker.Encryptor = EcbHacker.ecbEncryptorForTextAndKey(bytes ++ bytes ++ bytes, Old.randomBytes(16))
    EcbHacker.determineBlockLength(encryptor) shouldBe 16
  }

  "Hacker" should "determine first Byte" in {
    val encryptor : EcbHacker.Encryptor = EcbHacker.ecbEncryptorForTextAndKey(bytes, Old.randomBytes(16))
    EcbHacker.decodeNext(Array(), 16, encryptor) shouldBe Some(5.toByte)
  }

  "Hacker" should "determine second Byte" in {
    val encryptor : EcbHacker.Encryptor = EcbHacker.ecbEncryptorForTextAndKey(bytes, Old.randomBytes(16))
    EcbHacker.decodeNext(Array(5).map(_.toByte), 16, encryptor) shouldBe Some(9.toByte)
  }

  "Hacker" should "determine tenth Byte" in {
    val encryptor : EcbHacker.Encryptor = EcbHacker.ecbEncryptorForTextAndKey(bytes, Old.randomBytes(16))
    EcbHacker.decodeNext(Array(5, 9, 34, 23, 45, 65, 2, 3, 6).map(_.toByte), 16, encryptor) shouldBe Some(7.toByte)
  }

  "Hacker" should "determine sixteenth Byte" in {
    val encryptor : EcbHacker.Encryptor = EcbHacker.ecbEncryptorForTextAndKey(bytes, Old.randomBytes(16))
    EcbHacker.decodeNext(Array(5, 9, 34, 23, 45, 65, 2, 3, 6, 7, 8, 9, 34, 12, 24, 45).map(_.toByte), 16, encryptor) shouldBe Some(67.toByte)
  }

  "Hacker" should "determine final Byte" in {
    val encryptor : EcbHacker.Encryptor = EcbHacker.ecbEncryptorForTextAndKey(bytes, Old.randomBytes(16))
    EcbHacker.decodeNext(Array(5, 9, 34, 23, 45, 65, 2, 3, 6, 7, 8, 9, 34, 12, 24, 45, 67, 87).map(_.toByte), 16, encryptor) shouldBe Some(54.toByte)
  }

  "Hacker" should "end after final Byte" in {
    val encryptor : EcbHacker.Encryptor = EcbHacker.ecbEncryptorForTextAndKey(bytes, Old.randomBytes(16))
    EcbHacker.decodeNext(Array(5, 9, 34, 23, 45, 65, 2, 3, 6, 7, 8, 9, 34, 12, 24, 45, 67, 87, 54).map(_.toByte), 16, encryptor) shouldBe Some(1)
  }

  "Hacker" should "end after final Byt2e" in {
    val encryptor : EcbHacker.Encryptor = EcbHacker.ecbEncryptorForTextAndKey(bytes, Old.randomBytes(16))
    EcbHacker.decodeNext(Array(5, 9, 34, 23, 45, 65, 2, 3, 6, 7, 8, 9, 34, 12, 24, 45, 67, 87, 54, 1).map(_.toByte), 16, encryptor) shouldBe None
  }

  "Hacker" should "remove random prefix" in {
    val encryptor : EcbHacker.Encryptor = EcbHacker.ecbEncryptorForTextAndKeyWithRandom(bytes, Old.randomBytes(16))
    val encryptorWithoutRandom = EcbHacker.randomPrefixRemovingEncryptor(16, encryptor)
    EcbHacker.decodeNext(Array(), 16, encryptorWithoutRandom) shouldBe None
  }

  "Hacker" should "remove random prefix 2" in {
    val encryptor : EcbHacker.Encryptor = EcbHacker.ecbEncryptorForTextAndKeyWithRandom(bytes, Old.randomBytes(16))
    val encryptorWithoutRandom = EcbHacker.randomPrefixRemovingEncryptor(16, encryptor)
    EcbHacker.decodeNext(Array(5, 9, 34, 23, 45, 65, 2, 3, 6, 7, 8, 9, 34, 12, 24, 45).map(_.toByte), 16, encryptorWithoutRandom) shouldBe Some(67.toByte)
  }

  "Hacker" should "hack" in {
    val encryptor : EcbHacker.Encryptor = EcbHacker.ecbEncryptorForTextAndKeyWithRandom(bytes, Old.randomBytes(16))
    assert(EcbHacker.crackEncryptorWithRandomPrefix(encryptor).toList == bytes.toList)
  }

}
