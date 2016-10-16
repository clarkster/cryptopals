package clarkster.challenges

import clarkster._

object Challenge13_ECBCutAndPaste extends Challenge {
  override val number: Int = 13
  override val desc: String =
    """
      |Write a k=v parsing routine, as if for a structured cookie. The routine should take:
      |
      |foo=bar&baz=qux&zap=zazzle
      |... and produce:
      |
      |{
      |  foo: 'bar',
      |  baz: 'qux',
      |  zap: 'zazzle'
      |}
      |(you know, the object; I don't care if you convert it to JSON).
      |
      |Now write a function that encodes a user profile in that format, given an email address. You should have something like:
      |
      |profile_for("foo@bar.com")
      |... and it should produce:
      |
      |{
      |  email: 'foo@bar.com',
      |  uid: 10,
      |  role: 'user'
      |}
      |... encoded as:
      |
      |email=foo@bar.com&uid=10&role=user
      |Your "profile_for" function should not allow encoding metacharacters (& and =). Eat them, quote them, whatever you want to do, but don't let people set their email address to "foo@bar.com&role=admin".
      |
      |Now, two more easy functions. Generate a random AES key, then:
      |
      |Encrypt the encoded user profile under the key; "provide" that to the "attacker".
      |Decrypt the encoded user profile and parse it.
      |Using only the user input to profile_for() (as an oracle to generate "valid" ciphertexts) and the ciphertexts themselves, make a role=admin profile.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    def parse(str : String) : Map[String, String] = {
      str split '&' map(str => str.split("=")) map(pair => pair(0) -> pair(1)) toMap
    }

    def profileFor(str : String) : String = {
      encodeProfile(Map(
        "email" -> str,
        "uid" -> "10",
        "role" -> "user"
      ))
    }

    def encodeProfile(profile : Map[String, String]) : String =  {
      profile mapValues(_.replaceAll("&=", "")) map(pair => pair._1 + "=" + pair._2) mkString "&"
    }

    val key = rndKey(16)
    val ecb = Algorithm.ECB(key)
    val decrypt = Algorithm.ECB(key, padding=NoPadding, mode=Decrypt)

    def encryptProfileFor(str : String)  = {
      ecb(profileFor(str).bytes)
    }

    def decryptProfileFor(txt : List[Byte]) : Map[String, String] = {
      parse(decrypt(txt).ascii)
    }

    val encryptor : Algorithm.Encryptor = {
      byteList => encryptProfileFor(byteList.ascii)
    }

    val profile1 = encryptProfileFor("JoeBloggs@test.com")
    val blockSize = Oracle.detectECBBlockSize(encryptor)
    println(
      s"""
        |Regular user, decrypts to " + ${decryptProfileFor(profile1)}
        |Then analysing the individual blocks:
        |Block size is " + $blockSize
        |There's a 6 byte prefix (for email=), then our text, then a 18 byte suffix (for &uid=10&role=user)
        |So by using an email of length 13, and encrypting it, we will force user into a block of its own
        |2x16 byte crypto blocks for the preamble, and 1xblock of crypto(user) - padded
      """.stripMargin)


    val profile2 = encryptProfileFor("1234567890123")
    val profile3 = encryptProfileFor("1234567890" + "admin" + ByteListOps.copies(11, '=').ascii) // pad with a character we know will be strippeds

    println(
      s"""
         |Now we have two rogue profiles. One valid profile which allows us to force 'user' into a block of its own in
         |the Crypto output, and once which tells us the Crypyo for 'admin' (padded)
         |Our hack crypto text is constructed from the valid profile, with the last block replaced
      """.stripMargin)

    val target = profile2.take(32) ::: profile3.slice(16, 32)
    println("Decrypted profile is " + decryptProfileFor(target))
    assert(decryptProfileFor(target)("role") == "admin")
    println("And we're successfully admin")
  }
}
