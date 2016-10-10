package clarkster.challenges

import clarkster._

object Challenge30_BreakAnMD4KeyedMACUsingLengthExtension extends Challenge {
  override val number: Int = 30
  override val desc: String =
    """
      |Break an MD4 keyed MAC using length extension
      |Second verse, same as the first, but use MD4 instead of SHA-1. Having done this attack once against SHA-1, the MD4 variant should take much less time; mostly just the time you'll spend Googling for an implementation of MD4.
      |
      |You're thinking, why did we bother with this?
      |Blame Stripe. In their second CTF game, the second-to-last challenge involved breaking an H(k, m) MAC with SHA1. Which meant that SHA1 code was floating all over the Internet. MD4 code, not so much.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val md4 = MD4.withKey(Key(Helpers.randomLengthOfRandomBytes(2, 20)))

    val originalMessage = "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon".getBytes
    val newMessage = ";admin=true".getBytes

    //SHA1(key || original-message || glue-padding || new-message)

    val signed = md4.sign(originalMessage)

    // Guessed profile - originalMessage + padding + newMessage, where padding is based on the key length too
    def forgedProfile(keyLength : Int) : ByteList =  {
      val gluePadding = MD4Padding.padChars(originalMessage.length + keyLength)
      ByteList(originalMessage ++ gluePadding ++ newMessage)
    }

    // forge a signature, by taking the valid SHA1 state, and appending our new padded data
    def forgedSig(keyLength : Int) : ByteList = {
      val paddedExtraBlock = newMessage ++ MD4Padding.padChars(keyLength + forgedProfile(keyLength).length)
      MD4
        .fromSig(signed.bytes)
        .update(Block(paddedExtraBlock))
        .bytes
    }
    println("Guessing at key sizes. Identical code to last time, but targeted at the other algorithm")
    val (profile, sig) = (1 to 24)
        .map(i => (forgedProfile(i), forgedSig(i)))
        .find(profileAndSig => md4.verify(profileAndSig._1, profileAndSig._2))
        .get

    assert(md4.verify(profile, sig))
    assert(profile.ascii.contains(";admin=true"))
    println("Found a valid profile and signature")
    println("Profile " + profile.ascii)
    println("Sig " + sig.bytes)
  }
}
