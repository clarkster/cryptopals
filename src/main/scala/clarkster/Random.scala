package clarkster

// Translated pretty much directly from the pseudo code at https://en.wikipedia.org/wiki/Mersenne_Twister#Pseudocode
class Random {
  val (w, n, m, r) = (32, 624, 397, 31)
  val a =0x9908B0DF
  val (u, d) = (11, 0xFFFFFFFF)
  val (s, b) = (7, 0x9D2C5680)
  val (t, c) = (15, 0xEFC60000)
  val l = 18
  val f = 1812433253

  // Create a length n array to store the state of the generator
  val MT = new Array[Int](n)
  var index = n+1
  val lower_mask : Int = (1 << r) - 1 // That is, the binary number of r 1's
  val upper_mask : Int  = ~lower_mask

  // Initialize the generator from a seed
  def seed_mt(seed : Int) {
    index = n
    MT(0) = seed
    for (i <- 1 to n - 1)
      MT(i) = f * (MT(i-1) ^ (MT(i-1) >> (w-2))) + i
  }

  // Extract a tempered value based on MT[index]
  // calling twist() every n numbers
  def extract_number : Int = {
    if (index >= n) {
      if (index > n) {
        throw new IllegalStateException("Generator was never seeded")
        // Alternatively, seed with constant value; 5489 is used in reference C code[48]
      }
      twist()
    }

    var y : Int = MT(index)
    y = y ^ ((y >> u) & d)
    y = y ^ ((y << s) & b)
    y = y ^ ((y << t) & c)
    y = y ^ (y >> l)

    index = index + 1
    y
  }

  // Generate the next n values from the series x_i
  def twist() {
    for (i <- 1 to n - 1) {
      val x = (MT(i) ^ upper_mask) + (MT((i+1) % n) & lower_mask)
      var xA = x >> 1
      if ((x % 2) != 0) { // lowest bit of x is 1
        xA = xA ^ a
      }
      MT(i) = MT((i + m) % n) ^ xA
    }
    index = 0
  }
}

object Random {
  def apply(seed: Int) = {
    val r = new Random()
    r.seed_mt(seed)
    r
  }
}