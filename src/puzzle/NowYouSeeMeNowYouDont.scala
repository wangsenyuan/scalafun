package puzzle

/**
 * Created by senyuanwang on 14-10-12.
 */
object NowYouSeeMeNowYouDont extends App {

  trait A {
    val foo: Int
    val bar = 10
    println("In A: foo: " + foo + ", bar: " + bar)
  }

  class B extends A {
    override val bar = 11
    val foo: Int = 25
    println("In B: foo: " + foo + ", bar: " + bar)
  }

  class C extends B {
    //overriden val will be initialized once, before that, it will stay its default value.
    override val bar = 99
    println("In C: foo: " + foo + ", bar: " + bar)
  }

  println("new B")
  new B
  println("new C")
  new C
}
