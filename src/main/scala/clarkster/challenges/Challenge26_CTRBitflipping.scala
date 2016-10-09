package clarkster.challenges

import clarkster._

object Challenge26_CTRBitflipping extends Challenge {
  override val number: Int = 26
  override val desc: String =
    """
      |CTR bitflipping
      |There are people in the world that believe that CTR resists bit flipping attacks of the kind to which CBC mode is susceptible.
      |
      |Re-implement the CBC bitflipping exercise from earlier to use CTR mode instead of CBC mode. Inject an "admin=true" token.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val algorithm = CTR(Key.random(16), Block.random(8))

    def profile(userdata: String): CipherText = {
      val profile = "comment1=cooking%20MCs;userdata=" + userdata.replace(";=", "") + ";comment2=%20like%20a%20pound%20of%20bacon"
      algorithm.encrypt(profile.getBytes)
    }

    def isAdmin(encryptedProfile: CipherText): Boolean = {
      val bytes : ByteList = algorithm.decrypt(encryptedProfile)
      val profile = bytes.ascii
      profile.contains(";admin=true;")
    }


    val fixedText = "A" * 16 * 20
    val knownCipher = profile(fixedText)
    val knownStream = knownCipher.xOrRepeating(fixedText.map(_.toByte).toList).blocks

    println("Determined a few bytes of the CTR stream. (We can't guess the first 2 blocks)")

    val userProfile = profile("a@b.co") // 6 chars, so we end on an even block size
    assert(userProfile.length % 16 == 0)

    println("Take a valid profile")
    val nextCTCBlock = knownStream(userProfile.length / 16)
    val encryptedIsAdminSuffix = Block(";admin=true;".getBytes).xOr(nextCTCBlock, truncateToShortest = true)

    println("Add our evil suffix")

    val adminProfile = CipherText(userProfile.blocks :+ encryptedIsAdminSuffix)
    assert(isAdmin(adminProfile))
    println("And we're successfully admin")
  }
}
