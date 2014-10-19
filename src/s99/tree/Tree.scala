package s99.tree

/**
 * Created by senyuanwang on 14-10-7.
 */
sealed abstract class Tree[+T] {
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]

  def isMirrorOf[U >: T](that: Tree[U]): Boolean

  def isSymmetric: Boolean =
    this match {
      case End => true
      case Node(_, l, r) => l.isMirrorOf(r)
    }
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = {
    if (x < value) {
      Node(value, left.addValue(x), right)
    } else if (x > value) {
      Node(value, left, right.addValue(x))
    } else {
      this
    }
  }

  def isMirrorOf[U >: T](that: Tree[U]): Boolean =
    that match {
      case Node(_, l, r) => left.isMirrorOf(r) && right.isMirrorOf(l)
      case _ => false
    }

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  def addValue[U <% Ordered[U]](x: U) = Node(x)

  def isMirrorOf[T](that: Tree[T]) = that == End

  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
    case n if n < 1 => List(End)
    case n if n % 2 == 1 => {
      val subtrees = cBalanced(n / 2, value)
      subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
    }
    case n if n % 2 == 0 => {
      val lesserSubtrees = cBalanced((n - 1) / 2, value)
      val greaterSubtrees = cBalanced((n - 1) / 2 + 1, value)
      lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
    }
  }

  def fromList[T <% Ordered[T]](xs: List[T]): Tree[T] =
    xs.foldLeft(End: Tree[T])((t, x) => t.addValue(x))

  def symmetricBalancedTrees[T](n: Int, x: T): List[Tree[T]] =
    cBalanced(n, x).filter(_.isSymmetric)

  def hbalTrees[T](height: Int, x: T): List[Tree[T]] = height match {
    case n if n < 1 => List(End)
    case n if n == 1 => List(Node(x))
    case n =>
      val lessOne = hbalTrees(n - 1, x)
      val lessTwo = hbalTrees(n - 2, x)

      val subTree1 = for {
        a <- lessOne
        b <- lessOne
      } yield Node(x, a, b)

      val subTree2 = for {
        a <- lessOne
        b <- lessTwo
      } yield List(Node(x, a, b), Node(x, b, a))

      subTree1 ::: (subTree2.flatten)

  }
}