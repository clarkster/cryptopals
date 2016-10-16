package clarkster.challenges

import clarkster._

object Challenge29_BreakASHA1KeyedMACUsingLengthExtension extends Challenge {
  override val number: Int = 29
  override val desc: String =
    """
      |Break a SHA-1 keyed MAC using length extension
      |Secret-prefix SHA-1 MACs are trivially breakable.
      |
      |The attack on secret-prefix SHA1 relies on the fact that you can take the ouput of SHA-1 and use it as a new starting point for SHA-1, thus taking an arbitrary SHA-1 hash and "feeding it more data".
      |
      |Since the key precedes the data in secret-prefix, any additional data you feed the SHA-1 hash in this fashion will appear to have been hashed with the secret key.
      |
      |To carry out the attack, you'll need to account for the fact that SHA-1 is "padded" with the bit-length of the message; your forged message will need to include that padding. We call this "glue padding". The final message you actually forge will be:
      |
      |SHA1(key || original-message || glue-padding || new-message)
      |(where the final padding on the whole constructed message is implied)
      |
      |Note that to generate the glue padding, you'll need to know the original bit length of the message; the message itself is known to the attacker, but the secret key isn't, so you'll need to guess at it.
      |
      |This sounds more complicated than it is in practice.
      |
      |To implement the attack, first write the function that computes the MD padding of an arbitrary message and verify that you're generating the same padding that your SHA-1 implementation is using. This should take you 5-10 minutes.
      |
      |Now, take the SHA-1 secret-prefix MAC of the message you want to forge --- this is just a SHA-1 hash --- and break it into 32 bit SHA-1 registers (SHA-1 calls them "a", "b", "c", &c).
      |
      |Modify your SHA-1 implementation so that callers can pass in new values for "a", "b", "c" &c (they normally start at magic numbers). With the registers "fixated", hash the additional data you want to forge.
      |
      |Using this attack, generate a secret-prefix MAC under a secret key (choose a random word from /usr/share/dict/words or something) of the string:
      |
      |"comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"
      |Forge a variant of this message that ends with ";admin=true".
      |
      |This is a very useful attack.
      |For instance: Thai Duong and Juliano Rizzo, who got to this attack before we did, used it to break the Flickr API.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val sha = SHA.withKey(Key(rnd(2, 20)))

    val originalMessage = "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon".bytes
    val newMessage = ";admin=true".bytes

    //SHA1(key || original-message || glue-padding || new-message)

    val signed = sha.sign(originalMessage)

    // Guessed profile - originalMessage + padding + newMessage, where padding is based on the key length too
    def forgedProfile(keyLength : Int) : List[Byte] =  {
      val gluePadding = SHA1Padding.padChars(originalMessage.length + keyLength)
      originalMessage ++ gluePadding ++ newMessage
    }

    // forge a signature, by taking the valid SHA1 state, and appending our new padded data
    def forgedSig(keyLength : Int) : List[Byte] = {
      val paddedExtraBlock = newMessage ++ SHA1Padding.padChars(keyLength + forgedProfile(keyLength).length)
      SHA
        .fromSig(signed)
        .update(paddedExtraBlock)
        .bytes
    }
    println("Guessing at key sizes")
    val (profile, sig) = (1 to 24)
        .map(i => (forgedProfile(i), forgedSig(i)))
        .find(profileAndSig => sha.verify(profileAndSig._1, profileAndSig._2))
        .get

    assert(sha.verify(profile, sig))
    assert(profile.ascii.contains(";admin=true"))
    println("Found a valid profile and signature")
    println("Profile " + profile.ascii)
    println("Sig " + sig)
  }
}
