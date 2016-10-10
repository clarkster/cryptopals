package clarkster

import org.scalatest.{FlatSpec, Matchers}

class Sha1Spec extends FlatSpec with Matchers {

  "Sha1 Algorithm" should "match the java version" in {
    val testBytes = "This is a test message".getBytes
    val sha1 = SHA(testBytes)
    val javaSha = java.security.MessageDigest.getInstance("SHA-1").digest(testBytes)
    sha1.bytes shouldBe javaSha.toList
  }

  "Sha1 Algorithm" should "match the java version for a long message" in {
    val testBytes = ("This is a test message" * 100).getBytes
    val sha1 = SHA.sign(testBytes)
    val javaSha = java.security.MessageDigest.getInstance("SHA-1").digest(testBytes)
    sha1.bytes shouldBe javaSha.toList
  }

  "Sha1 Algorithm" should "match the java version for full alphabet" in {
    val testBytes = ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789").getBytes
    val sha1 = SHA.sign(testBytes)
    val javaSha = java.security.MessageDigest.getInstance("SHA-1").digest(testBytes)
    sha1.bytes shouldBe javaSha.toList
  }


  "Sha1 Algorithm" should "be decomposable" in {
    val key = Key("B" * 8)
    val sha = SHA.withKey(key)

    val originalMessage = ("A" * 80).getBytes
    val newMessage = ("C" * 10).getBytes

    //SHA1(key || original-message || glue-padding || new-message)

    val signed = sha.sign(originalMessage)
    val gluePadded = originalMessage ++ SHA1Padding.padChars(originalMessage.length + key.length)

    // ExtraBlock is constructed as our new fragment, padded to the full length of the entire message
    val paddedExtraBlock = newMessage ++ SHA1Padding.padChars(key.length + gluePadded.length + newMessage.length)

    val forgedSig = SHA.fromSig(signed.bytes).update(Block(paddedExtraBlock))

    assert(sha.verify(gluePadded ++ newMessage, forgedSig.bytes))
  }

  "Sha1 state" should "be reusable" in {
    val testBytes = "This is a test message".getBytes
    val sig1 = SHA(testBytes)
    val state2 = SHA.fromSig(sig1.bytes)
    state2.bytes shouldBe sig1.bytes
  }

  "MD4 state" should "be reusable" in {
    val testBytes = "This is a test message".getBytes
    val sig1 = MD4(testBytes)
    val state2 = MD4.fromSig(sig1.bytes)
    state2.bytes shouldBe sig1.bytes
  }

}
