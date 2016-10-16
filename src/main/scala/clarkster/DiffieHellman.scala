package clarkster

case class PrivateKey(k : BigInt)
case class PublicKey(k : BigInt)
case class DiffieHellmanParams(p : BigInt, g : BigInt)
case class KeyPair(priv : PrivateKey, pub : PublicKey)

class DiffieHellman(params: DiffieHellmanParams) {

  def generate(seed : Int) : KeyPair = {
    val a = seed.abs % params.p
    val A = params.g.modPow(a, params.p)
    KeyPair(PrivateKey(a), PublicKey(A))
  }

  def session(myKey : PrivateKey, theirKey : PublicKey): BigInt =
    theirKey.k.modPow(myKey.k, params.p)

  def sessionECBKey(myKey : PrivateKey, theirKey : PublicKey): Key =
    session(myKey, theirKey).toByteArray.digest(SHA).take(16).key

}

object DiffieHellman {

  val defaultP = BigInt("""
             |ffffffffffffffffc90fdaa22168c234c4c6628b80dc1cd129024
             |e088a67cc74020bbea63b139b22514a08798e3404ddef9519b3cd
             |3a431b302b0a6df25f14374fe1356d6d51c245e485b576625e7ec
             |6f44c42e9a637ed6b0bff5cb6f406b7edee386bfb5a899fa5ae9f
             |24117c4b1fe649286651ece45b3dc2007cb8a163bf0598da48361
             |c55d39a69163fa8fd24cf5f83655d23dca3ad961c62f356208552
             |bb9ed529077096966d670c354e4abc9804f1746c08ca237327fff
             |fffffffffffff"""".stripMargin.replace("\n", "").getBytes)
  val defaultG = 2
  val defaultParams = DiffieHellmanParams(defaultP, defaultG)

  def apply() : DiffieHellman = apply(defaultParams)

  def apply(params : DiffieHellmanParams) : DiffieHellman =
    new DiffieHellman(params)
}
