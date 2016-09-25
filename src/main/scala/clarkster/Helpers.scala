package clarkster

object Helpers {

  def bytesToString(bytes: Array[Byte]) = bytes map(_.toChar) mkString
}
