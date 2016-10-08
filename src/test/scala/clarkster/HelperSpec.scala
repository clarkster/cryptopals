package clarkster

import org.scalatest.{FlatSpec, Matchers}

class HelperSpec extends FlatSpec with Matchers {

  "Hamming distance" should "calculate correctly" in {
    val val1 = Block("this is a test".getBytes.toList)
    val val2 = Block("wokka wokka!!!".getBytes.toList)
    assert(Helpers.hammingDistance(val1, val2) == 37)
  }
}
