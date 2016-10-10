package clarkster

import org.scalatest.{FlatSpec, Matchers}

class EcbHackerSpec extends FlatSpec with Matchers {

  val bytes : List[Byte] = List(5, 9, 34, 23, 45, 65, 2, 3, 6, 7, 8, 9, 34, 12, 24, 45, 67, 87, 54).map(_.toByte)
  val encryptor = Algorithms.withSecretAppended(bytes, ECB(Key.random(16)).encrypt)

  "Suffix detector" should "determine first Byte" in {
    EcbHacker.decodeNext(Array(), 16, encryptor) shouldBe Some(5.toByte)
  }

  it should "determine second Byte" in {
    EcbHacker.decodeNext(Array(5).map(_.toByte), 16, encryptor) shouldBe Some(9.toByte)
  }

  it should "determine tenth Byte" in {
    EcbHacker.decodeNext(Array(5, 9, 34, 23, 45, 65, 2, 3, 6).map(_.toByte), 16, encryptor) shouldBe Some(7.toByte)
  }

  it should "determine sixteenth Byte" in {
    EcbHacker.decodeNext(Array(5, 9, 34, 23, 45, 65, 2, 3, 6, 7, 8, 9, 34, 12, 24, 45).map(_.toByte), 16, encryptor) shouldBe Some(67.toByte)
  }

  it should "determine final Byte" in {
    EcbHacker.decodeNext(Array(5, 9, 34, 23, 45, 65, 2, 3, 6, 7, 8, 9, 34, 12, 24, 45, 67, 87).map(_.toByte), 16, encryptor) shouldBe Some(54.toByte)
  }

  it should "get pad char after final Byte" in {
    EcbHacker.decodeNext(Array(5, 9, 34, 23, 45, 65, 2, 3, 6, 7, 8, 9, 34, 12, 24, 45, 67, 87, 54).map(_.toByte), 16, encryptor) shouldBe Some(1)
  }

  it should "end after pad char after final Byte" in {
    EcbHacker.decodeNext(Array(5, 9, 34, 23, 45, 65, 2, 3, 6, 7, 8, 9, 34, 12, 24, 45, 67, 87, 54, 1).map(_.toByte), 16, encryptor) shouldBe None
  }


  it should "Determine whole suffix" in {
    EcbHacker.crackEncryptor(encryptor) shouldBe bytes
  }


  val encryptorWithRandom = Algorithms.withRandomPrepended(16, encryptor)


  "Prefix Remover" should "remove random prefix" in {
    val withoutRandom = EcbHacker.randomPrefixRemovingEncryptor(16, encryptorWithRandom)
    val original = encryptor.apply(bytes)
    val removed = withoutRandom.apply(bytes)
    original.hex shouldBe removed.hex
  }

  it should "remove random prefix 2" in {
    val encryptorWithoutRandom = EcbHacker.randomPrefixRemovingEncryptor(16, encryptorWithRandom)
    EcbHacker.decodeNext(Array(5, 9, 34, 23, 45, 65, 2, 3, 6, 7, 8, 9, 34, 12, 24, 45).map(_.toByte), 16, encryptorWithoutRandom) shouldBe Some(67.toByte)
  }

  it should "determine suffix" in {
    assert(EcbHacker.crackEncryptorWithRandomPrefix(encryptorWithRandom).toList == bytes)
  }

}
