package puzzle

/**
 * Created by senyuanwang on 14/11/16.
 */
object PrivateLives extends App {

  val test = new Private

  class Private {
    //    def foo1: Any = new Private.C1

    //    def foo2: Private.C2 = new Private.C2

    def foo3: Any = new Private.C2
  }

  object Private {

    class C1 private {}

    private class C2 {}

  }

  //  test.foo1
  test.foo3

}
