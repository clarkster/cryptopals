package clarkster.challenges

import clarkster._

object Challenge16_CBCBitflippingAttacks extends Challenge {
  override val number: Int = 16

  override val desc: String =
    """
      |CBC bitflipping attacks
      |Generate a random AES key.
      |
      |Combine your padding code and CBC code to write two functions.
      |
      |The first function should take an arbitrary input string, prepend the string:
      |
      |"comment1=cooking%20MCs;userdata="
      |.. and append the string:
      |
      |";comment2=%20like%20a%20pound%20of%20bacon"
      |The function should quote out the ";" and "=" characters.
      |
      |The function should then pad out the input to the 16-byte AES block length and encrypt it under the random AES key.
      |
      |The second function should decrypt the string and look for the characters ";admin=true;" (or, equivalently, decrypt, split the string on ";", convert each resulting string into 2-tuples, and look for the "admin" tuple).
      |
      |Return true or false based on whether the string exists.
      |
      |If you've written the first function properly, it should not be possible to provide user input to it that will generate the string the second function is looking for. We'll have to break the crypto to do that.
      |
      |Instead, modify the ciphertext (without knowledge of the AES key) to accomplish this.
      |
      |You're relying on the fact that in CBC mode, a 1-bit error in a ciphertext block:
      |
      |Completely scrambles the block the error occurs in
      |Produces the identical 1-bit error(/edit) in the next ciphertext block.
      |Stop and think for a second.
      |Before you implement this attack, answer this question: why does CBC mode have this property?
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val key = rndKey(16)
    val iv = rnd(16)
    val encrypt = Algorithm.CBC(key, iv, Encrypt)
    val decrypt = Algorithm.CBC(key, iv, Decrypt)

    def profile(userdata: String): List[Byte] = {
      val profile = "comment1=cooking%20MCs;userdata=" + userdata.replace(";=", "") + ";comment2=%20like%20a%20pound%20of%20bacon"
      encrypt(profile.bytes)
    }

    def isAdmin(encryptedProfile: List[Byte]): Boolean = {
      val bytes = decrypt(encryptedProfile)
      val profile = bytes.ascii
      profile.contains(";admin=true;")
    }

    val probeBlock = "0123456789ABCDEF".bytes
    println("Starting with probeBlock " + probeBlock.ascii)
    val forceAdminBlock = "0123;admin=true;".bytes
    println("We would like to use a block to force admin as " + forceAdminBlock.ascii)

    val forceAdminSanitised = "01230admin0true0".bytes
    println("We sanitise this by replacing the prohibited chars " + forceAdminSanitised.ascii)

    val flipper = forceAdminSanitised.xOr(forceAdminBlock)
    println("And the operation to do so is an XOR to switch the bits " + flipper.hex)


    val goodProfile = profile(probeBlock.ascii + forceAdminSanitised.ascii)

    val (preamble : List[List[Byte]], remainder: List[List[Byte]]) = goodProfile.blocks(16).splitAt(2)  // We happen to know the preamble is exactly two blocks worth

    println("Rogue encrypted profile, with same bits flipped in the probe crypto block")
    val rogueProfile = preamble ::: List(remainder.head.xOr(flipper).bytes) ::: remainder.tail

    assert(isAdmin(rogueProfile.flatten))
    println("And we're successfully admin")
  }
}
