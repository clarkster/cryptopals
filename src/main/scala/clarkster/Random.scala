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

    val tempered = temper(MT(index))
    index = index + 1
    tempered
  }

  def temper(y : Int) = {
    temper_debug(y)._4
  }

  def temper_debug(y : Int) = {
    val y1 = temper1(y)
    val y2 = temper2(y1)
    val y3 = temper3(y2)
    val y4 = temper4(y3)
    (y1, y2, y3, y4)
  }

  def temper1: Int => Int = y => y ^ ((y >>> u) & d)
  def temper2: Int => Int = y => y ^ ((y << s) & b)
  def temper3: Int => Int = y => y ^ ((y << t) & c)
  def temper4: Int => Int = y => y ^ (y >>> l)

  def untemper4: Int => Int = y => y ^ (y >>> l)
  def untemper3: Int => Int = y => y ^ (y << t) & c
  def untemper2: Int => Int = y => {
      var res = y // bits 6-0 are already OK
      res = y ^ ((res << 7) & b) // bits 14-0 will be OK
      res = y ^ ((res << 7) & b) //bits 24-0  will be OK
      res = y ^ ((res << 7) & b) //bits 30-0  will be OK
      res = y ^ ((res << 7) & b) //bits 32-0  will be OK& b)
      res
  }

  def untemper1: Int => Int = y =>  {
    var res = y // bits 32-21 are already OK
    res = y ^ (res >>> 11) // bits 32-10 will be OK
    res = y ^ (res >>> 11) // bits 32-0 will be OK
    res
  }

  def untemper(y4: Int) = {
    val y3 = untemper4(y4)
    val y2 = untemper3(y3)
    val y1 = untemper2(y2)
    val y = untemper1(y1)
    y
  }

  // Generate the next n values from the series x_i
  def twist() {
    for (i <- 1 until n) {
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