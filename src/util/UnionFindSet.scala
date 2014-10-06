package util

/**
 * Created by senyuanwang on 14-10-6.
 */
class UnionFindSet[A: ID](val map: Map[Int, Int]) {
  private val ev = implicitly[ID[A]]
  var internalMap = map.withDefault(x => x)

  def this() = this(Map.empty)

  def find(a: A): Int = find(ev.id(a))

  def ?(a: A) = find(a)

  def union(x: A, y: A): UnionFindSet[A] = union(ev.id(x), ev.id(y))

  def union(x: Int, y: Int): UnionFindSet[A] = {
    val px = find(x)
    val py = find(y)
    new UnionFindSet[A](internalMap + (px -> py))
  }

  def ?(p: (A, A)) = {
    val px = find(p._1)
    val py = find(p._2)
    px == py
  }

  def +(p: (A, A)) = union(p._1, p._2)

  private def find(x: Int): Int = {
    val p = internalMap(x)
    if (p == x) {
      p
    } else {
      internalMap += (x -> find(p))
      internalMap(x)
    }
  }
}


trait ID[A] {
  def id(a: A): Int
}

object UnionFindSet {
  implicit def intID = new ID[Int] {
    def id(a: Int) = a
  }

  def apply[A: ID]() = new UnionFindSet[A]()
}