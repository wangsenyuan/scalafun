package s99

/**
 * Created by senyuanwang on 14-10-7.
 */
object LogicOp {

  def nand(a: => Boolean, b: => Boolean) = not(and(a, b))

  def nor(a: => Boolean, b: => Boolean) = not(or(a, b))

  def or(a: => Boolean, b: => Boolean) = if (a) true else b

  def not(a: => Boolean) = if (a) false else true

  def xor(a: Boolean, b: Boolean): Boolean = not(equ(a, b))

  def equ(a: Boolean, b: Boolean): Boolean = or(and(a, b), and(not(a), not(b)))

  def and(a: => Boolean, b: => Boolean) = if (a) b else false

  def impl(a: => Boolean, b: => Boolean): Boolean = or(not(a), b)

  def table2(f: (Boolean, Boolean) => Boolean) {
    println("A     B     result")
    for {a <- List(true, false);
         b <- List(true, false)} {
      printf("%-5s %-5s %-5s\n", a, b, f(a, b))
    }
  }

  implicit def toLogicOp(a: => Boolean) = new LogicOp(a)
}


class LogicOp(val a: Boolean) extends AnyVal {
  def and(b: => Boolean) = LogicOp.and(a, b)

  def not = LogicOp.not(a)

  def or(b: => Boolean) = LogicOp.or(a, b)

  def xor(b: => Boolean) = LogicOp.xor(a, b)

  def impl(b: => Boolean) = LogicOp.impl(a, b)

  def nand(b: => Boolean) = LogicOp.nand(a, b)

  def equ(b: => Boolean) = LogicOp.equ(a, b)
}