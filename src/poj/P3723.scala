package poj

import util.UnionFindSet

import scala.io.ReadStdin

/**
 * Created by senyuanwang on 14-10-6.
 */
object P3723 extends App {
  val T = ReadStdin.readLine().toInt

  def process(t: Int): Unit =
    if (t <= T) {
      ReadStdin.readLine() //the blank line
      val line = ReadStdin.readLine().split("\\s+").map(_.toInt)
      val n = line(0)
      val m = line(1)
      val r = line(2)

      var es = List.empty[(Int, Int, Int)]
      for (i <- 0 until r) {
        val l = ReadStdin.readLine().split("\\s+").map(_.toInt)
        es = (l(0), n + l(1), -l(2)) :: es
      }

      es = es.sortBy(x => x._3)

      import util.UnionFindSet._
      val y = kruskal(es, UnionFindSet(), 0)

      println(10000 * (n + m) + y)

      process(t + 1)
    }

  def kruskal(es: List[(Int, Int, Int)], uf: UnionFindSet[Int], sum: Int): Int =
    es match {
      case Nil => sum
      case (x, y, d) :: tail if (uf ? (x -> y)) => kruskal(tail, uf, sum)
      case (x, y, d) :: tail => kruskal(tail, uf + (x -> y), sum + d)
    }

  process(1)
}
