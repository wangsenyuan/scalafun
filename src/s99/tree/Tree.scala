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

  def layoutBinaryTree = layoutBinaryTreeInternal(1, 1)._1

  def layoutBinaryTreeInternal(x: Int, y: Int): (Tree[T], Int)

  def treeDepth: Int

  def leftmostNodeDepth: Int

  def layoutBinaryTree2: Tree[T] = {
    val d = treeDepth
    val x0 = (2 to leftmostNodeDepth).map((n) => Math.pow(2, d - n).toInt).reduceLeft(_ + _) + 1
    layoutBinaryTree2Internal(x0, 1, d - 2)
  }

  def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int): Tree[T]

  def bounds: List[(Int, Int)]

  def layoutBinaryTree3: Tree[T] =
    layoutBinaryTree3Internal(bounds.map(_._1).reduceLeft(_ min _) * -1 + 1, 1)

  def layoutBinaryTree3Internal(x: Int, depth: Int): Tree[T]
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

  override def toString = (left, right) match {
    case (End, End) => s"$value"
    case (_, _) => s"$value($left,$right)"
  }

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

  def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int) = {
    val (leftTree, myX) = left.layoutBinaryTreeInternal(x, depth + 1)
    val (rightTree, nextX) = right.layoutBinaryTreeInternal(myX + 1, depth + 1)
    (PositionedNode(value, leftTree, rightTree, myX, depth), nextX)
  }

  def treeDepth: Int = (left.treeDepth max right.treeDepth) + 1

  def leftmostNodeDepth: Int = left.leftmostNodeDepth + 1

  def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int): Tree[T] =
    PositionedNode(
      value,
      left.layoutBinaryTree2Internal(x - Math.pow(2, exp).toInt, depth + 1, exp - 1),
      right.layoutBinaryTree2Internal(x + Math.pow(2, exp).toInt, depth + 1, exp - 1),
      x, depth)

  def bounds: List[(Int, Int)] = {
    def lowerBounds = (left.bounds, right.bounds) match {
      case (Nil, Nil) => Nil
      case (lb, Nil) => lb.map((b) => (b._1 - 1, b._2 - 1))
      case (Nil, rb) => rb.map((b) => (b._1 + 1, b._2 + 1))
      case (lb, rb) => {
        val shift = lb.zip(rb).map((e) => (e._1._2 - e._2._1) / 2 + 1).reduceLeft(_ max _)
        lb.map(Some(_)).zipAll(rb.map(Some(_)), None, None).map(_ match {
          case (Some((a, b)), Some((c, d))) => (a - shift, d + shift)
          case (Some((a, b)), None) => (a - shift, b - shift)
          case (None, Some((c, d))) => (c + shift, d + shift)
          case (None, None) => throw new Exception // Placate the compiler; can't get here.
        })
      }
    }
    (0, 0) :: lowerBounds
  }

  def layoutBinaryTree3Internal(x: Int, depth: Int): Tree[T] = bounds match {
    case _ :: (bl, br) :: _ => PositionedNode(
      value, left.layoutBinaryTree3Internal(x + bl, depth + 1),
      right.layoutBinaryTree3Internal(x + br, depth + 1), x, depth)
    case _ => PositionedNode(value, End, End, x, depth)
  }

}

case object End extends Tree[Nothing] {
  def addValue[U <% Ordered[U]](x: U) = Node(x)

  def isMirrorOf[T](that: Tree[T]) = that == End

  override def toString = ""

  override def size: Int = 0

  override def leafCount: Int = 0

  override def internalList: List[Nothing] = Nil

  override def leafList: List[Nothing] = Nil

  override def atLevel(level: Int): List[Nothing] = Nil

  def layoutBinaryTreeInternal(x: Int, depth: Int) = (End, x)

  def treeDepth: Int = 0

  def leftmostNodeDepth: Int = 0

  def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int) = End

  def bounds: List[(Int, Int)] = Nil

  def layoutBinaryTree3Internal(x: Int, depth: Int) = End
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T],
                         x: Int, y: Int) extends Node[T](value, left, right) {
  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
}

object PositionedNode {
  def apply[T](value: T, left: Tree[T], right: Tree[T], x: Int, y: Int) = new PositionedNode[T](value, left, right, x, y)
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

  def fromString(str: String): Tree[Char] = {
    def split(x: String, i: Int, level: Int): (String, String) =
      if (i >= x.length) {
        ("", "")
      } else {
        val c = x.charAt(i)
        if (c == ',' && level == 0) {
          (x.substring(0, i), x.substring(i + 1))
        } else if (c == '(') {
          split(x, i + 1, level + 1)
        } else if (c == ')') {
          split(x, i + 1, level - 1)
        } else {
          split(x, i + 1, level)
        }
      }

    if (str.isEmpty) End
    else if (str.size == 1) Node(str.charAt(0), End, End)
    else {
      val c = str.charAt(0)
      val (left, right) = split(str.substring(2, str.length - 1), 0, 0)
      Node(c, fromString(left), fromString(right))
    }
  }

}