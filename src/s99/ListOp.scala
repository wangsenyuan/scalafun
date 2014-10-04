package s99

import scala.collection.mutable.ListBuffer

object ListOp {
  def last[A](list: List[A]): Option[A] =
    list match {
      case Nil => None
      case h :: Nil => Some(h)
      case h :: tail => last(tail)
    }

  def penultimate[A](list: List[A]): Option[A] =
    list match {
      case Nil => None
      case h :: Nil => None
      case h :: x :: Nil => Some(h)
      case h :: tail => penultimate(tail)
    }

  def nth[A](index: Int, list: List[A]): Option[A] =
    (index, list) match {
      case (0, h :: tail) => Some(h)
      case (x, _) if x < 0 => None
      case (_, Nil) => None
      case (x, h :: tail) => nth(x - 1, tail)
    }

  def length[A](list: List[A]): Int = {
    def go(cnt: Int, list: List[A]): Int =
      list match {
        case Nil => cnt
        case h :: tail => go(1 + cnt, tail)
      }
    go(0, list)
  }

  def reverse[A](list: List[A]): List[A] = {
    def go(acc: List[A], left: List[A]): List[A] =
      left match {
        case Nil => acc
        case h :: tail => go(h :: acc, tail)
      }

    go(Nil, list)
  }

  def isPalindrome[A](list: List[A]): Boolean = {
    val r = reverse(list)
    list.zip(r).forall(x => x._1 == x._2)
  }

  //  def flatten[A](ll: List[List[A]]): List[A] = {
  //    def go(lb: ListBuffer[A], ll: List[List[A]]): List[A] =
  //      ll match {
  //        case Nil => lb.toList
  //        case l :: tail =>
  //          go(lb ++ l, tail)
  //      }
  //
  //    go(ListBuffer(), ll)
  //  }

  def flatten(xs: List[Any]): List[Any] = {
    def go(lb: ListBuffer[Any], xs: List[Any]): List[Any] =
      xs match {
        case Nil => lb.toList
        case (x: List[Any]) :: tail =>
          go(lb ++ flatten(x), tail)
        case x :: tail =>
          go(lb ++ List(x), tail)
      }
    go(ListBuffer(), xs)
  }

  def compress[A](xs: List[A]): List[A] = {
    def go(ys: List[A], xs: List[A]): List[A] =
      (ys, xs) match {
        case (_, Nil) => ys.reverse
        case (y :: _, x :: xtail) if x == y => go(ys, xtail)
        case (_, x :: tail) => go(x :: ys, tail)
      }

    go(Nil, xs)
  }
}
