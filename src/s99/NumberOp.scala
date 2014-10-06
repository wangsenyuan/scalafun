package s99

/**
 * Created by senyuanwang on 14-10-6.
 */
class IntOp(val x: Int) {

  import s99.NumberOp._

  def isPrime: Boolean =
    (x > 1) && (primes takeWhile {
      _ <= Math.sqrt(x)
    } forall {
      x % _ != 0
    })

  def totient: Int = (2 to x).foldLeft(1)((cnt, y) => if (isCoprimeTo(y)) cnt + 1 else cnt)

  def isCoprimeTo(y: Int): Boolean = gcd(x, y) == 1

  def phi: Int = {
    primeFactorMultiplicity.foldLeft(1)(
      (rs, nc) => {
        val (p, m) = nc
        rs * (p - 1) * pow(p, m - 1)
      }
    )
  }

  def primeFactors = NumberOp.primeFactors(x)

  def primeFactorMultiplicity = {
    import s99.ListOp._
    encode(primeFactors) map {
      _.swap
    }
  }

  def goldbach: (Int, Int) = {
    val ps = primes takeWhile {
      _ < x
    }

    ps find { p => ps.contains(x - p)} match {
      case None => throw new IllegalArgumentException
      case Some(p1) => (p1, x - p1)
    }
  }

  private def pow(a: Int, b: Int): Int =
    if (b == 0) 1
    else if (b == 1) a
    else a * pow(a, b - 1)

}

object NumberOp {
  val primes = 2 #:: (Stream.from(3, 2) filter {
    _.isPrime
  })

  implicit def toIntOp(x: Int) = new IntOp(x)

  def gcd(x: Int, y: Int): Int =
    if (y == 0) x
    else gcd(y, x % y)

  def primeFactors(x: Int): List[Int] = {
    def primeFactorsR(n: Int, ps: Stream[Int]): List[Int] =
      if (n.isPrime) List(n)
      else if (n % ps.head == 0) ps.head :: primeFactorsR(n / ps.head, ps)
      else primeFactorsR(n, ps.tail)

    primeFactorsR(x, primes)
  }

  def listPrimesinRange(r: Range): List[Int] = {
    primes.takeWhile(x => x <= r.end).toList.dropWhile(_ < r.start)
  }

  def goldbachListLimited(r: Range, limit: Int): List[(Int, Int)] =
    goldbachList(r).filter(x => x._1 > 50 && x._2 > 50)

  def goldbachList(r: Range): List[(Int, Int)] =
    (for {x <- r if x > 2 && x % 2 == 0} yield x.goldbach).toList
}