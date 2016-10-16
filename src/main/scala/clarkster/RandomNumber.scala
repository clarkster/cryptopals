package clarkster

import scala.annotation.tailrec

case class RandomState(index: Int, MT: Seq[Int]) {
  def next = RandomNumber.next(this)
}

object RandomNumber {
  val N = 624
  val lower_mask: Int = (1 << 31) - 1
  val upper_mask: Int = ~lower_mask

  private def apply(index: Int, MT: Seq[Int]) = RandomState(index, MT)


  def next(state: RandomState): (Int, RandomState) = state.index match {
    case N => next(twist(state))
    case i => (temper(state.MT(i)), RandomState(i + 1, state.MT))
  }

  // Generate the next n values from the series x_i
  def twist(state: RandomState): RandomState = {
    @tailrec
    def twistRange(i : Int, MT: Seq[Int]): Seq[Int] = {
      i match {
        case N => MT
        case _ =>
          val x = (MT(i) ^ upper_mask) + (MT((i+1) % N) & lower_mask)
          var xA = x >> 1
          if ((x % 2) != 0) { // lowest bit of x is 1
            xA = xA ^ 0x9908B0DF
          }
          val newMT = MT.updated(i, MT((i + 397) % N) ^ xA)
          twistRange(i + 1, newMT)
      }
    }
    RandomState(0, twistRange(1, state.MT))
  }

  def seed(seed: Int): RandomState = {
    @tailrec
    def seedRange(i : Int, MT: Seq[Int]): Seq[Int] = {
      i match {
        case N => MT
        case _ =>
          val current = 1812433253 * (MT(i - 1) ^ (MT(i - 1) >> (32 - 2))) + i
          seedRange(i + 1, MT.updated(i, current))
      }
    }
    val mt = seedRange(1, Seq.fill(N)(0).updated(0, seed))
    RandomState(N, mt)
  }

  def seq(fromState: RandomState): Stream[(Int, RandomState)] = {
    val x = next(fromState)
    x #:: seq(x._2)
  }

  def randomDigits(fromSeed : RandomState, len : Int) : List[Int] = {
    seq(fromSeed).take(len).map(_._1).toList
  }

  val temper1: Int => Int = y => y ^ ((y >>> 11) & 0xFFFFFFFF)
  val temper2: Int => Int = y => y ^ y << 7 & 0x9D2C5680
  val temper3: Int => Int = y => y ^ ((y << 15) & 0xEFC60000)
  val temper4: Int => Int = y => y ^ (y >>> 18)
  val tempers = temper1 :: temper2 :: temper3 :: temper4 :: Nil
  def temper(y: Int): Int = tempers.foldLeft(y)((soFar, transform) => transform.apply(soFar))

  def untemper4: Int => Int = y => y ^ (y >>> 18)
  def untemper3: Int => Int = y => y ^ (y << 15) & 0xEFC60000
  def untemper2: Int => Int = y => {
    var res = y // bits 6-0 are already OK
    res = y ^ ((res << 7) & 0x9D2C5680) // bits 14-0 will be OK
    res = y ^ ((res << 7) & 0x9D2C5680) //bits 24-0  will be OK
    res = y ^ ((res << 7) & 0x9D2C5680) //bits 30-0  will be OK
    res = y ^ ((res << 7) & 0x9D2C5680) //bits 32-0  will be OK& b)
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
}

object RandomNumberImpure {
  var randomState = RandomNumber.seed(System.currentTimeMillis().toInt)
  def next() : Int = {
    val numAndState = randomState.next
    randomState = numAndState._2
    numAndState._1
  }
}