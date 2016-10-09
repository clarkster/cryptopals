package clarkster

import org.scalatest.{FlatSpec, Matchers}
import SHA1.SHA1State._

class Sha1Spec extends FlatSpec with Matchers {

  "Sha1 Algorithm" should "match the java version" in {
    val testBytes = "This is a test message".getBytes
    val sha1 = SHA1(testBytes)
    val javaSha = java.security.MessageDigest.getInstance("SHA-1").digest(testBytes)
    sha1.bytes shouldBe javaSha.toList
  }
}
