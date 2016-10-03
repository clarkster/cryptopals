package clarkster.challenges

import clarkster.{EcbHacker, Helpers}

object Challenge14_ByteAtATimeECBDecryption_Harder extends Challenge {
  override val number: Int = 14
  override val desc: String =
    """
      |Take your oracle function from #12. Now generate a random count of random bytes and prepend this string to every plaintext. You are now doing:
      |
      |AES-128-ECB(random-prefix || attacker-controlled || target-bytes, random-key)
      |Same goal: decrypt the target-bytes.
      |
      |Stop and think for a second.
      |What's harder than challenge #12 about doing this? How would you overcome that obstacle? The hint is: you're using all the tools you already have; no crazy math is required.
      |
      |Think "STIMULUS" and "RESPONSE".
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    println("Detecting... (this one's a bit slow)")
    val encryptor : EcbHacker.Encryptor = EcbHacker.ecbEncryptorForTextAndKeyWithRandom(EcbHacker.secretBytes.bytes.toArray, EcbHacker.fixedSecretKey)
    println (Helpers.bytesToString(EcbHacker.crackEncryptorWithRandomPrefix(encryptor)))
  }
}
