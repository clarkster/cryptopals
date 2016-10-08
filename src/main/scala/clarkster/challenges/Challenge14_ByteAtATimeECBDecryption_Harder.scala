package clarkster.challenges

import clarkster._


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

    val ecb = ECB(key = Key.random(16))
    val secretText =
      """
        |Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg
        |aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq
        |dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg
        |YnkK
      """.stripMargin
    val secret = ByteList.fromBase64(secretText)

    val secretAppended = Algorithms.withSecretAppended(secret, ecb.encrypt)
    val encryptor = Algorithms.withRandomPrepended(256, secretAppended)


    println (Helpers.bytesToString(EcbHacker.crackEncryptorWithRandomPrefix(encryptor)))
  }
}
