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

  def size: Int

  def leafCount: Int

  def internalList: List[T]

  def leafList: List[T]

  def atLevel(level: Int): List[T]
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

  override def size: Int = 1 + left.size + right.size

  override def leafCount: Int = (left, right) match {
    case (End, End) => 1
    case (l, r) => l.leafCount + r.leafCount
  }

  def internalList: List[T] = (left, right) match {
    case (End, End) => Nil
    case _ => value :: left.internalList ::: right.internalList
  }

  def leafList: List[T] = (left, right) match {
    case (End, End) => List(value)
    case _ => left.leafList ::: right.leafList
  }

  override def atLevel(level: Int): List[T] =
    level match {
      case 0 => Nil
      case 1 => List(value)
      case x => left.atLevel(x - 1) ::: right.atLevel(x - 1)
    }
}

case object End extends Tree[Nothing] {
  def addValue[U <% Ordered[U]](x: U) = Node(x)

  def isMirrorOf[T](that: Tree[T]) = that == End

  override def toString = "."

  override def size: Int = 0

  override def leafCount: Int = 0

  override def internalList: List[Nothing] = Nil

  override def leafList: List[Nothing] = Nil

  override def atLevel(level: Int): List[Nothing] = Nil
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

  def minHbalNodes(height: Int): Int = {
    def go: Stream[Int] =
      0 #:: 1 #:: go.zip(go.tail).map(x => x._1 + x._2 + 1)

    go.take(height + 1).last
  }

  def minHbalHeight(nodes: Int): Int =
    if (nodes == 0) 0
    else minHbalHeight(nodes / 2) + 1

  def maxHbalHeight(nodes: Int): Int = {
    def go: Stream[Int] =
      0 #:: 1 #:: go.zip(go.tail).map(x => x._1 + x._2 + 1)

    go.zipWithIndex.takeWhile(x => x._1 <= nodes).last._2
  }

  def hbalTreesWithNodes[T](nodes: Int, value: T): List[Tree[T]] =
    (minHbalHeight(nodes) to maxHbalHeight(nodes)).flatMap(hbalTrees(_, value)).filter(_.size == nodes).toList

  def completeBinaryTree[T](nodes: Int, value: T): Tree[T] = {
    def go(x: Int): Tree[T] =
      if (x > nodes) End
      else Node(value, go(2 * x), go(2 * x + 1))

    go(1)
  }
}