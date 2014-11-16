package puzzle

/**
 * Created by senyuanwang on 14/11/8.
 */
object CompainObjectCreationTest extends App {

  val bOfA = B[A](new A)
}


class A

class B[T](val t: T)

object B {
  def apply[T](t: => T) = new B[T](t)
}

class C[+T](val value: T, children: List[C[T]]) {
  /*  def this(value: T) = this(value, Nil)

    def this[U >: T](value: U) = this(value, Nil)*/

  def replace[U >: T](t: U) = new C(t, children)
}

object C {
  def apply[T](value: T) = new C(value, Nil)

}