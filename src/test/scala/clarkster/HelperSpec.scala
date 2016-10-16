package clarkster

import org.scalatest.{FlatSpec, Matchers}

class HelperSpec extends FlatSpec with Matchers {

  "Hamming distance" should "calculate correctly" in {
    val val1 = "this is a test".bytes
    val val2 = "wokka wokka!!!".bytes
    assert(val1.hammingDistance(val2) == 37)
  }
}
