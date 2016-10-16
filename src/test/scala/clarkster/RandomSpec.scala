package clarkster

import org.scalatest.{FlatSpec, Matchers}

class RandomSpec extends FlatSpec with Matchers {

  "temper 4" should "be reversible" in {
    RandomNumber.untemper4(RandomNumber.temper4(-1234567)) shouldBe -1234567
  }

  "temper3" should "be reversible" in {
    RandomNumber.untemper3(RandomNumber.temper3(5)) shouldBe 5
    RandomNumber.untemper3(RandomNumber.temper3(345678901)) shouldBe 345678901
    RandomNumber.untemper3(RandomNumber.temper3(-1234567)) shouldBe -1234567
  }

  "temper2" should "be reversible" in {
    RandomNumber.untemper2(RandomNumber.temper2(345678901)) shouldBe 345678901
    RandomNumber.untemper2(RandomNumber.temper2(-1234567)) shouldBe -1234567
  }

  "temper1" should "be reversible" in {
    RandomNumber.untemper1(RandomNumber.temper1(5)) shouldBe 5
    RandomNumber.untemper1(RandomNumber.temper1(345678901)) shouldBe 345678901
    RandomNumber.untemper1(RandomNumber.temper1(-1234567)) shouldBe -1234567
  }

  "temper" should "be reversible" in {
    RandomNumber.untemper(RandomNumber.temper(5)) shouldBe 5
    RandomNumber.untemper(RandomNumber.temper(345678901)) shouldBe 345678901
    RandomNumber.untemper(RandomNumber.temper(-1234567)) shouldBe -1234567
  }
}
