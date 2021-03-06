package clarkster.challenges

import clarkster._
import clarkster.Algorithm._

import scala.util.{Random, Try}

object Challenge17_CBCPaddingOracle extends Challenge {
  override val number: Int = 17
  override val desc: String =
    """
      |The CBC padding oracle
      |This is the best-known attack on modern block-cipher cryptography.
      |
      |Combine your padding code and your CBC code to write two functions.
      |
      |The first function should select at random one of the following 10 strings:
      |
      |MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc=
      |MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic=
      |MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw==
      |MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg==
      |MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl
      |MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA==
      |MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw==
      |MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8=
      |MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g=
      |MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93
      |... generate a random AES key (which it should save for all future encryptions), pad the string out to the 16-byte AES block size and CBC-encrypt it under that key, providing the caller the ciphertext and IV.
      |
      |The second function should consume the ciphertext produced by the first function, decrypt it, check its padding, and return true or false depending on whether the padding is valid.
      |
      |What you're doing here.
      |This pair of functions approximates AES-CBC encryption as its deployed serverside in web applications; the second function models the server's consumption of an encrypted session token, as if it was a cookie.
      |
      |It turns out that it's possible to decrypt the ciphertexts provided by the first function.
      |
      |The decryption here depends on a side-channel leak by the decryption function. The leak is the error message that the padding is valid or not.
      |
      |You can find 100 web pages on how this attack works, so I won't re-explain it. What I'll say is this:
      |
      |The fundamental insight behind this attack is that the byte 01h is valid padding, and occur in 1/256 trials of "randomized" plaintexts produced by decrypting a tampered ciphertext.
      |
      |02h in isolation is not valid padding.
      |
      |02h 02h is valid padding, but is much less likely to occur randomly than 01h.
      |
      |03h 03h 03h is even less likely.
      |
      |So you can assume that if you corrupt a decryption AND it had valid padding, you know what that padding byte is.
      |
      |It is easy to get tripped up on the fact that CBC plaintexts are "padded". Padding oracles have nothing to do with the actual padding on a CBC plaintext. It's an attack that targets a specific bit of code that handles decryption. You can mount a padding oracle on any CBC block, whether it's padded or not.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    val strs = List("MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc=",
                    "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic=",
                    "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw==",
                    "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg==",
                    "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl",
                    "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA==",
                    "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw==",
                    "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8=",
                    "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g=",
                    "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93")

    val iv = rnd(16)
    val key = rndKey(16)
    val encrypt = CBC(key, iv, Encrypt)
    val decrypt = CBC(key, iv, Decrypt, NoPadding)

    def cipherText : List[Byte] = {
      val str = strs(Random.nextInt(10)).b64
      encrypt(str)
    }

    def isPaddingValid(cipherText: List[Byte] ) : Boolean = {
      Try(PKCS7.unpad(decrypt(cipherText))) isSuccess
    }
    // Reference http://robertheaton.com/2013/07/29/padding-oracle-attack
    // For a pair of crypted blocks, we can target the second one by carefully setting up a pair of blocks and determining
    // if the padding is valid.
    def determineBlock(prevBlock : List[Byte], currentBlock : List[Byte]): String = {
      var intermediateState: List[Byte] = List()
      for (i <- 1 to 16) {
        val found = Helpers.eachByte.find(b => {
          val start = List.fill(16 - i)('A'.toByte) :+ b
          val ending = intermediateState.map(b => (b ^ i).toByte)
          val block = start ::: ending
          val test = block ::: currentBlock
          isPaddingValid(test)
        })
        intermediateState = (found.get ^ i).toByte :: intermediateState
      }
      intermediateState.xOr(prevBlock).ascii
    }


    // Wording of the challenge wasn't quite clear - as a hacker, have we obtained the IV too?  If so, we can
    // get to the first block of plain text
    val firstBlock = determineBlock(iv, cipherText.take(16))

    // Otherwise we can get to all except the first
    val decrypted = cipherText.grouped(16).sliding(2, 1).map(blocks => determineBlock(blocks.head, blocks(1))).mkString

    println(firstBlock + decrypted)

  }
}
