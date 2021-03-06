package clarkster

import org.scalatest.{FlatSpec, Matchers}
import scala.io.Source

class ScoreSpec extends FlatSpec with Matchers {

  "Example String" should "frequency analysis" in {
    val charNStr = Score.bestSingleChar("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736".hex)
    charNStr shouldBe ('X', "Cooking MC's like a pound of bacon", 596)
  }

  "Score" should "be highest for e" in {
    Score.score("e") shouldBe 25
  }




}
