package clarkster.challenges

import akka.actor.FSM._
import akka.actor._
import clarkster._

object Challenge34_ImplementAMITMKeyFixingAttackonDiffieHellmanWithParameterInjection extends Challenge {
  override val number: Int = 34
  override val desc: String =
    """
      |Use the code you just worked out to build a protocol and an "echo" bot. You don't actually have to do the network part of this if you don't want; just simulate that. The protocol is:
      |
      |A->B
      |Send "p", "g", "A"
      |B->A
      |Send "B"
      |A->B
      |Send AES-CBC(SHA1(s)[0:16], iv=random(16), msg) + iv
      |B->A
      |Send AES-CBC(SHA1(s)[0:16], iv=random(16), A's msg) + iv
      |(In other words, derive an AES key from DH with SHA1, use it in both directions, and do CBC with random IVs appended or prepended to the message).
      |
      |Now implement the following MITM attack:
      |
      |A->M
      |Send "p", "g", "A"
      |M->B
      |Send "p", "g", "p"
      |B->M
      |Send "B"
      |M->A
      |Send "p"
      |A->M
      |Send AES-CBC(SHA1(s)[0:16], iv=random(16), msg) + iv
      |M->B
      |Relay that to B
      |B->M
      |Send AES-CBC(SHA1(s)[0:16], iv=random(16), A's msg) + iv
      |M->A
      |Relay that to A
      |M should be able to decrypt the messages. "A" and "B" in the protocol --- the public keys, over the wire --- have been swapped out with "p". Do the DH math on this quickly to see what that does to the predictability of the key.
      |
      |Decrypt the messages from M's vantage point as they go by.
      |
      |Note that you don't actually have to inject bogus parameters to make this attack work; you could just generate Ma, MA, Mb, and MB as valid DH parameters to do a generic MITM attack. But do the parameter injection attack; it's going to come up again.
    """.stripMargin


  override def main(args: Array[String]): Unit = {

    case class  StartMessage(destination: ActorRef)
    case class KeyAndParams(params : DiffieHellmanParams, key : PublicKey)
    case class EncryptedMessage(message : List[Byte], iv : List[Byte])

    sealed trait KeyExchangeState
    case object InitialState extends KeyExchangeState
    case object ExchangingState extends KeyExchangeState
    case object ExchangedState extends KeyExchangeState
    case class Data(params : DiffieHellmanParams, sessionKey : Either[KeyPair, Key])

    def encrypt(key : Key, message : List[Byte]) : EncryptedMessage = {
      val iv = rnd(16)
      EncryptedMessage(message.apply(Algorithm.CBC(key, iv)), iv)
    }

    class Alice extends Actor with FSM[KeyExchangeState, Data] {

      startWith(InitialState,
               Data(DiffieHellman.defaultParams, Left(DiffieHellman().generate(System.currentTimeMillis().toInt))))

      when(InitialState) {
        case Event(StartMessage(destination), Data(params, myKeys)) =>
          println("A Received Start")
          destination ! KeyAndParams(params, myKeys.left.get.pub)
          goto(ExchangingState) using Data(params, myKeys)
      }

      when(ExchangingState) {
        case Event(PublicKey(k), Data(params, myKeys)) =>
          val key = DiffieHellman(params).sessionECBKey(myKeys.left.get.priv, PublicKey(k))
          println("Alice Received key: " + key.hex)
          sender ! encrypt(key, "I love you".bytes)
          goto(ExchangedState) using Data(params, Right(key))
      }

      when(ExchangedState) {
        case Event(EncryptedMessage(message, iv), data) =>
          val decrypted = message.apply(Algorithm.CBC(data.sessionKey.right.get, iv, mode=Decrypt))
          println("Alice Received secret message: " + decrypted.ascii)
          context.system.terminate()
          stop(Normal)
      }
    }

    class Bob extends Actor with FSM[KeyExchangeState, Data] {
      startWith(InitialState,
        Data(DiffieHellman.defaultParams,
             Left(DiffieHellman().generate(System.currentTimeMillis().toInt * 2))))

      when (InitialState) {
         case Event(KeyAndParams(params, theirKey), Data(_, myKeys)) =>
           val key = DiffieHellman(params).sessionECBKey(myKeys.left.get.priv, theirKey)
           println("Bob Received Key: " + key.hex)
           sender ! myKeys.left.get.pub
           goto(ExchangedState) using Data(params, Right(key))
      }


      when (ExchangedState) {
        case Event(EncryptedMessage(message, iv), data) =>
          // decrypt secret message
          val decrypted = message.apply(Algorithm.CBC(data.sessionKey.right.get, iv, mode=Decrypt))
          println("Bob Received secret message: " + decrypted.ascii)
          sender ! encrypt(data.sessionKey.right.get, decrypted ++ decrypted)
          stop(Normal)
      }
    }

    class Malory(bob : ActorRef) extends Actor {
      var alice : ActorRef = _
      var params : DiffieHellmanParams = _
      // By forcing the public key to 'p' we get a session key of (p ^ something) % p === 0
      val rogueKey = List(0.toByte).digest(SHA).take(16).key
      println("Malory using key " + rogueKey.hex)

      override def receive: Receive = {
        case KeyAndParams (ps, key) =>
          params = ps
          alice = sender
          println("Malory sends Key and params")
          bob ! KeyAndParams (ps, PublicKey(params.p))
        case key @ PublicKey(_) =>
          println("Malory sends PublicKey")
          alice ! PublicKey(params.p)
        case EncryptedMessage(m, iv) =>
          val decrypted = m.apply(Algorithm.CBC(rogueKey, iv, mode=Decrypt))
          if (sender == alice) {
            println("Malory message to bob " + decrypted.ascii)
            bob ! EncryptedMessage(m, iv)
          } else {
            println("Malory message to alice " + decrypted.ascii)
            alice ! EncryptedMessage(m, iv)
          }
      }
    }

    val system = ActorSystem("MessageSystem")

    val bob = system.actorOf(Props(new Bob), name = "Bob")
    val alice = system.actorOf(Props(new Alice), name = "Alice")

    println("Normal exchange starts...")
    alice ! StartMessage(bob)

    Thread.sleep(1000) // how to wait?

    val system2 = ActorSystem("MessageSystem")

    val bob2 = system2.actorOf(Props(new Bob), name = "Bob")
    val alice2 = system2.actorOf(Props(new Alice), name = "Alice")

    println("Corrupt exchange starts...")
    val malory = system2.actorOf(Props(new Malory(bob2)), name = "Malory")
    alice2 ! StartMessage(malory)
  }
}
