package clarkster

object CookieParser {

  // deliberately naive to start with
  def parse(str : String) : Map[String, String] = {
    str.split('&').map(str => str.split("=")).map(pair => pair(0) -> pair(1)) toMap
  }

  def profileFor(str : String) : String = {
    return encodeProfile(Map(
      ("email" -> str),
      ("uid" -> "10"),
      ("role" -> "user")
    ))
  }

  def encodeProfile(profile : Map[String, String]) : String =  {
    return profile.mapValues(v => v.replaceAll("&=", "")).map(pair => pair._1 + "=" + pair._2) mkString "&"
  }




}
