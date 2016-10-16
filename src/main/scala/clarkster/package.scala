package object clarkster {
  implicit def stringToStringEncoding(str : String) : StringEncodingOps = new StringEncodingOps(str)
  implicit def stringEncodingToString(encoded : StringEncodingOps) : String = encoded.str
  implicit def bytesToByteList(bytes : List[Byte]) : ByteListOps = ByteListOps(bytes)
  implicit def byteArrayToByteList(bytes : Array[Byte]) : ByteListOps = ByteListOps(bytes)
  implicit def byteListToBytes(byteList : ByteListOps) : List[Byte] = byteList.bytes
  implicit def byteListToArray(byteList : ByteListOps) : Array[Byte] = byteList.bytes.toArray
  implicit def intWithTimes(n : Int) = new {
    def times(f : => Unit) = 1 to n foreach(_ => f)
  }
  def rnd(n : Int) : List[Byte] =
    List.fill(n)(scala.util.Random.nextInt(256)) map (_.toByte)

  def rnd(nMin : Int, nMax : Int) : List[Byte] =
    rnd(scala.util.Random.nextInt(nMax - nMin) + nMin)

  def rndKey(n : Int) : Key = new Key(rnd(n))
}

