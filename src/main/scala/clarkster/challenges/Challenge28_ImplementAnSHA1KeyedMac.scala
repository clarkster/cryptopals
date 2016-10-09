package clarkster.challenges

import clarkster.{ByteList, Key, SHA1}

object Challenge28_ImplementAnSHA1KeyedMac extends Challenge {
  override val number: Int = 28
  override val desc: String =
    """
      |Implement a SHA-1 keyed MAC
      |Find a SHA-1 implementation in the language you code in.
      |
      |Don't cheat. It won't work.
      |Do not use the SHA-1 implementation your language already provides (for instance, don't use the "Digest" library in Ruby, or call OpenSSL; in Ruby, you'd want a pure-Ruby SHA-1).
      |Write a function to authenticate a message under a secret key by using a secret-prefix MAC, which is simply:
      |
      |SHA1(key || message)
      |Verify that you cannot tamper with the message without breaking the MAC you've produced, and that you can't produce a new MAC without knowing the secret key.
    """.stripMargin

  override def main(args: Array[String]): Unit = {
    def sha1WithKey(key : Key, message : ByteList) = {
      SHA1(key.bytes ++ message.bytes)
    }

    val valid = sha1WithKey(Key("Yellow Submarine"), ByteList.fromAscii("My Secret Message"))
    val tampered = sha1WithKey(Key("Yellow Submarine"), ByteList.fromAscii("My Secret Massage"))

    assert(valid != tampered)

    val tampered2 = sha1WithKey(Key("Yellow Wubmarine"), ByteList.fromAscii("My Secret Message"))

    assert(valid != tampered2)
    println("Validated SHA1")
  }
}
