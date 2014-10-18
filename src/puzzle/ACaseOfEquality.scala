package puzzle

/**
 * Created by senyuanwang on 14-10-18.
 *
 * For case classes, the equals, hashCode and toString methods are generated, and for two instances of a case class with the same elements, we could expect that both equals and hashCode return the same result (i.e. answers 1 and 4).

 * According to the SLS (§5.3.2) however, a case class implicitly overrides the methods equals, hashCode and toString of class scala.AnyRef only if the case class itself does not provide a definition for one of these methods and only if a concrete definition is not given in some base class of the case class (except AnyRef).

 * In our example, the base trait SpyOnEquals of CCSpy provides an equals method, so the case class does not provide its own definition and the comparison of two different instances returns false.

 * For method hashCode however no implementation is provided, neither in class CCSpy nor in some base class, so here the implicitly overridden version is used which returns the same value for equal elements.

 * For case class CC, no definition of either equals or hashCode is provided, so the implicitly overridden versions are used for both methods. Mixing in SpyOnEquals when creating instances of the case classes does not affect this.ß
 */
object ACaseOfEquality extends App {

  val cc1 = new CC() with SpyOnEquals
  val cc2 = new CC() with SpyOnEquals
  val ccspy1 = CCSpy()
  val ccspy2 = CCSpy()

  trait SpyOnEquals {
    override def equals(x: Any) = {
      println("DEBUG: In equals");
      super.equals(x)
    }
  }

  case class CC()

  case class CCSpy() extends SpyOnEquals

  println(cc1 == cc2)
  println(cc1.## == cc2.##) // null-safe hashCode()
  println(ccspy1 == ccspy2)
  println(ccspy1.## == ccspy2.##)
}
