package s99

/**
 * Created by senyuanwang on 14-10-7.
 */
object Other {

  def gray(n: Int): List[String] = {
    import scala.collection.mutable.Map
    val cache = Map(0 -> List(""))
    def go(n: Int): List[String] = cache.get(n) match {
      case Some(l) => l
      case None =>
        val l = go(n - 1)
        cache += n -> (l.map("0" + _) ++ l.reverse.map("1" + _))
        cache(n)
    }

    go(n)
  }
}
