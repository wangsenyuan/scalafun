package s99

import scala.collection.mutable.ListBuffer
import scala.util.Random

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

  def isPalindrome[A](list: List[A]): Boolean = {
    val r = reverse(list)
    list.zip(r).forall(x => x._1 == x._2)
  }

  def reverse[A](list: List[A]): List[A] = {
    def go(acc: List[A], left: List[A]): List[A] =
      left match {
        case Nil => acc
        case h :: tail => go(h :: acc, tail)
      }

    go(Nil, list)
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

  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span {
        _ == ls.head
      }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def encode[A](ls: List[A]): List[(Int, A)] = {
    val packed = pack(ls)
    packed.map(x => (x.length, x.head))
  }

  def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    def go(xs: List[(Int, A)], ls: List[A]): List[(Int, A)] =
      (xs, ls) match {
        case (_, Nil) => xs.reverse
        case ((n, x) :: xtail, h :: tail) if x == h => go((n + 1, x) :: xtail, tail)
        case (_, h :: tail) => go((1, h) :: xs, tail)
      }
    go(Nil, ls)
  }

  def duplicate[A](ls: List[A]): List[A] = {
    decode(ls.map(x => (2, x)))
  }

  def duplicateN[A](n: Int, ls: List[A]): List[A] = {
    require(n > 0)
    decode(ls.map(x => (n, x)))
  }

  def decode[A](ls: List[(Int, A)]): List[A] = {
    ls.flatMap(x => List.fill(x._1)(x._2))
  }

  def drop[A](n: Int, ls: List[A]): List[A] = {
    require(n > 0)
    def go(m: Int, ls: List[A]): List[A] = {
      (m, ls) match {
        case (_, Nil) => Nil
        case (1, h :: tail) => go(n, tail)
        case (x, h :: tail) => h :: go(x - 1, tail)
      }
    }
    go(n, ls)
  }

  def slice[A](l: Int, k: Int, ls: List[A]): List[A] = {
    val (_, right) = split(l, ls)
    val (result, _) = split(k - l, right)
    result
  }

  def split[A](n: Int, ls: List[A]): (List[A], List[A]) = {
    require(n >= 0 && n <= ls.length)
    def go(left: List[A], right: List[A], n: Int): (List[A], List[A]) = {
      (right, n) match {
        case (_, 0) => (left.reverse, right)
        case (h :: tail, x) => go(h :: left, tail, x - 1)
      }
    }
    go(Nil, ls, n)
  }

  def rotate[A](n: Int, xs: List[A]): List[A] =
    if (n > xs.length) rotate(n % xs.length, xs)
    else if (n < 0) rotate(n + xs.length, xs)
    else xs.drop(n) ::: xs.take(n)

  def insertAt[A](a: A, n: Int, xs: List[A]): List[A] = {
    val (pre, tail) = split(n, xs)
    pre ::: (a :: tail)
  }

  def lotto(n: Int, m: Int): List[Int] = {
    val r = range(1, m)
    randomSelect(n, r)
  }

  def range(l: Int, r: Int): List[Int] = {
    def go(xs: List[Int], x: Int): List[Int] =
      if (x < l) xs
      else go(x :: xs, x - 1)

    go(Nil, r)
  }

  def randomSelect[A](n: Int, xs: List[A]): List[A] = {
    def go(n: Int, xs: List[A], selected: List[A]): List[A] =
      if (n == 0) selected
      else {
        val (left, a) = removeAt(Random.nextInt(xs.length), xs)
        go(n - 1, left, a :: selected)
      }

    go(n, xs, Nil)
  }

  def removeAt[A](n: Int, xs: List[A]): (List[A], A) = {
    val (pre, tail) = split(n, xs)
    (pre ::: tail.tail, tail.head)
  }

  def randomPermute[A](xs: List[A]): List[A] =
    randomSelect(xs.length, xs)

  def flatMapSublists[A, B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {
        sl.head :: _
      }
    }

  def group3[A](ls: List[A]): List[List[List[A]]] =
    for {
      a <- combinations(2, ls)
      noA = ls.diff(a)
      b <- combinations(3, noA)
    } yield List(a, b, noA.diff(b))

  def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
    case Nil => List(Nil)
    case n :: ns => combinations(n, ls) flatMap { c =>
      group(ns, ls.diff(c)) map {
        c :: _
      }
    }
  }

  def lsort[A](ll: List[List[A]]): List[List[A]] = {
    ll.sortBy(_.length)
  }

  def lsortFreq[A](ll: List[List[A]]): List[List[A]] = {
    val g = ll.groupBy(_.length)
    g.keys.toList.sortBy(g(_).size).flatMap(g(_))
  }
}
