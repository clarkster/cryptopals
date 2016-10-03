package clarkster

import org.scalatest.{FlatSpec, Matchers}


class CookieParserSpec  extends FlatSpec with Matchers {

  "Parser" should "parse example string" in {
    CookieParser.parse("foo=bar&baz=qux&zap=zazzle") shouldBe Map("foo" -> "bar", "baz" -> "qux", "zap" -> "zazzle")
  }
}