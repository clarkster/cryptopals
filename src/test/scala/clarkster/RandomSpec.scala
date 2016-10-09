package clarkster

import org.scalatest.{FlatSpec, Matchers}

class RandomSpec extends FlatSpec with Matchers {

  "temper 4" should "be reversible" in {
    val rnd = Random(100)
    //rnd.untemper4(rnd.temper4(5)) shouldBe 5
    //rnd.untemper4(rnd.temper4(345678901)) shouldBe 345678901
    println("expected " + Helpers.dump(-1234567))
    rnd.untemper4(rnd.temper4(-1234567)) shouldBe -1234567
  }

  "temper3" should "be reversible" in {
    val rnd = Random(100)
    rnd.untemper3(rnd.temper3(5)) shouldBe 5
    rnd.untemper3(rnd.temper3(345678901)) shouldBe 345678901
    rnd.untemper3(rnd.temper3(-1234567)) shouldBe -1234567
  }

  "temper2" should "be reversible" in {
    val rnd = Random(100)
   // rnd.untemper2(rnd.temper2(5)) shouldBe 5
    rnd.untemper2(rnd.temper2(345678901)) shouldBe 345678901
    rnd.untemper2(rnd.temper2(-1234567)) shouldBe -1234567
  }

  "temper1" should "be reversible" in {
    val rnd = Random(100)
    rnd.untemper1(rnd.temper1(5)) shouldBe 5
    rnd.untemper1(rnd.temper1(345678901)) shouldBe 345678901
    rnd.untemper1(rnd.temper1(-1234567)) shouldBe -1234567
  }

  "temper" should "be reversible" in {
    val rnd = Random(100)
    rnd.untemper(rnd.temper(5)) shouldBe 5
    rnd.untemper(rnd.temper(345678901)) shouldBe 345678901
    rnd.untemper(rnd.temper(-1234567)) shouldBe -1234567
  }


}
