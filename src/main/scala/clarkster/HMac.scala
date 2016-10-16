package clarkster

object HMac {
  def apply(key : Key, message : List[Byte]) : List[Byte] = {
    var keyBytes = key.bytes
    if (keyBytes.length > 64) {
      keyBytes = SHA(key.bytes).bytes // keys longer than blocksize are shortened
    }
    if (keyBytes.length < 64) {
      // keys shorter than blocksize are zero-padded (where ∥ is concatenation)
      keyBytes = key.bytes ++ ByteListOps.blank(64 - key.length)
    }

    val o_key_pad = ByteListOps.copies(64, 0x5c).xOr(keyBytes) // Where blocksize is that of the underlying hash function
    val i_key_pad = ByteListOps.copies(64, 0x36).xOr(keyBytes) // Where ⊕ is exclusive or (XOR)

    SHA(o_key_pad.bytes ++ SHA(i_key_pad.bytes ++ message.bytes).bytes) // Where ∥ is concatenation
  }
}
