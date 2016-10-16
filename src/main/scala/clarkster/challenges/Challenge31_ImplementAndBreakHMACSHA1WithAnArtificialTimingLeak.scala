package clarkster.challenges

import clarkster._
import net.roeim.minihttpserver.MiniHttpServer

import scala.annotation.tailrec
import scalaj.http.{Http, HttpResponse}

object Challenge31_ImplementAndBreakHMACSHA1WithAnArtificialTimingLeak extends Challenge {
  override val number: Int = 31
  override val desc: String =
    """
      |Implement and break HMAC-SHA1 with an artificial timing leak
      |The psuedocode on Wikipedia should be enough. HMAC is very easy.
      |
      |Using the web framework of your choosing (Sinatra, web.py, whatever), write a tiny application that has a URL that takes a "file" argument and a "signature" argument, like so:
      |
      |http://localhost:9000/test?file=foo&signature=46b4ec586117154dacd49d664e5d63fdc88efb51
      |Have the server generate an HMAC key, and then verify that the "signature" on incoming requests is valid for "file", using the "==" operator to compare the valid MAC for a file with the "signature" parameter (in other words, verify the HMAC the way any normal programmer would verify it).
      |
      |Write a function, call it "insecure_compare", that implements the == operation by doing byte-at-a-time comparisons with early exit (ie, return false at the first non-matching byte).
      |
      |In the loop for "insecure_compare", add a 50ms sleep (sleep 50ms after each byte).
      |
      |Use your "insecure_compare" function to verify the HMACs on incoming requests, and test that the whole contraption works. Return a 500 if the MAC is invalid, and a 200 if it's OK.
      |
      |Using the timing leak in this application, write a program that discovers the valid MAC for any file.
      |
      |Why artificial delays?
      |Early-exit string compares are probably the most common source of cryptographic timing leaks, but they aren't especially easy to exploit. In fact, many timing leaks (for instance, any in C, C++, Ruby, or Python) probably aren't exploitable over a wide-area network at all. To play with attacking real-world timing leaks, you have to start writing low-level timing code. We're keeping things cryptographic in these challenges.
    """.stripMargin

  override def main(args: Array[String]): Unit = {

    def insecure_compare(sig1: List[Byte], sig2: List[Byte]) = {
      sig1.zip(sig2).forall {
        case (b1, b2) =>
          Thread.sleep(50)
          b1 == b2
      }
    }

    class TestServer extends MiniHttpServer {
      val key = rndKey(64)
      var firstTime = true
      get("/test") { exchange =>
        val params = CookieParser.parse(exchange.getRequestURI.getQuery)
        val file = params("file")
        val sig = params("signature").hex
        val validSig = HMac(key, file.bytes)
        if (firstTime) {
          println("Actual" + validSig.hex)
          firstTime = false
        }
        if (!insecure_compare(sig, validSig)) {
          throw new IllegalArgumentException
        }
        file
      }
    }

    def checkSignature(sig : String) : Boolean = {
      val response : HttpResponse[String] = Http("http://localhost:8080/test")
        .param("file","test")
        .param("signature", sig)
        .asString

      response.code == 200
    }

    def timeFor(guess : String) : Long = {
      val t = System.currentTimeMillis()
      checkSignature(guess) // Don't even care about response code
      System.currentTimeMillis() - t
    }

    val KEYLEN = 20

    @tailrec
    def guessRecursive(pos: Int, soFar: String) : String = {
      println("Guessing character at " + pos + " soFar " + soFar)
      // bruteforce all characters. Could maybe instead use a fold function
      // to stop when a guess took significantly longer than the others
      pos match {
        case KEYLEN => soFar
        case _ =>
          val next : String = Helpers
          .eachByte
          .map(Hex.byteToHex)
          .map(bts => soFar + bts)
          .map(bts => (bts, timeFor(bts + "00" * (KEYLEN-pos))))
          .maxBy(_._2)
          ._1
        guessRecursive(pos + 1, next)
      }
    }

    println("Warning, test takes a v long time to run...")
    val server = new TestServer
    try {
      server.start()

      // warmup server else first req is slow.
      checkSignature("")

      val key = guessRecursive(0, "")

      assert(checkSignature(key))

    } finally {
      server.stop()
    }
  }

}
