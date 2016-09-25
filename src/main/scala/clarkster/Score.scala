package clarkster

object Score {

  // https://en.wikipedia.org/wiki/Letter_frequency.  Least most->least common
  val freqs = " etaoinshrdlcumwfgypbvkjxqz".reverse

  private def scoreChar(char: Char) = {
    val freq = freqs.indexOf(char)
    if (freq < 0) 0 else freq
  }
  /**
    * Score a string according to likelihood based on English character frequencies.
    * High score = more likely
    */
  def score(msg : String): Int = {
    msg.toLowerCase.toCharArray.foldLeft(0)((score, char) => score + scoreChar(char))
  }
}
