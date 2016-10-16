package clarkster.challenges

import clarkster._
import clarkster.Algorithm._

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
    val algorithm = CTR(rndKey(16), rnd(8))

    def profile(userdata: String): List[Byte] = {
      val profile = "comment1=cooking%20MCs;userdata=" + userdata.replace(";=", "") + ";comment2=%20like%20a%20pound%20of%20bacon"
      algorithm(profile.bytes)
    }

    def isAdmin(encryptedProfile: List[Byte]): Boolean = {
      val bytes = algorithm(encryptedProfile)
      val profile = bytes.ascii
      profile.contains(";admin=true;")
    }


    val fixedText = "A" * 16 * 20
    val knownCipher = profile(fixedText)
    val knownStream = knownCipher.xOrRepeating(fixedText.map(_.toByte).toList).blocks(16)

    println("Determined a few bytes of the CTR stream. (We can't guess the first 2 blocks)")

    val userProfile = profile("a@b.co") // 6 chars, so we end on an even block size
    assert(userProfile.length % 16 == 0)

    println("Take a valid profile")
    val nextCTCBlock = knownStream(userProfile.length / 16)
    val encryptedIsAdminSuffix = ";admin=true;".bytes.xOr(nextCTCBlock)

    println("Add our evil suffix")

    val adminProfile = userProfile ::: encryptedIsAdminSuffix
    assert(isAdmin(adminProfile))
    println("And we're successfully admin")
  }
}
