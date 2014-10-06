package s99

/**
 * Created by senyuanwang on 14-10-6.
 */
object NumberApp extends App {

  import s99.NumberOp._

  println(7.isPrime)

  println(8.isPrime)

  println(gcd(36, 63))

  println(35.isCoprimeTo(64))

  println(10.totient)

  println(315.primeFactors)

  println(315.primeFactorMultiplicity)

  def test(f: => Unit): Unit = {
    val start = System.currentTimeMillis()
    f
    val end = System.currentTimeMillis()
    println(s"it takes ${end - start} milliseconds to process.")
  }

  test {
    println(10090.totient)
  }

  test {
    println(10090.phi)
  }

  println(listPrimesinRange(7 to 31))

  println(28.goldbach)

  println(goldbachList(9 to 20))

  println(goldbachListLimited(1 to 2000, 50))
}
