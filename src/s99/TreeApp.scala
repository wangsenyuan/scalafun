package s99

/**
 * Created by senyuanwang on 14-10-7.
 */
object TreeApp extends App {

  import s99.tree.Tree._
  import s99.tree._

  println(cBalanced(4, 'x))

  println(fromList(List(3, 2, 5, 7, 1)))

  println(fromList(List(3, 2, 5, 7, 1)).isSymmetric)

  println(fromList(List(3, 2, 5, 7, 4)).isSymmetric)

  println(symmetricBalancedTrees(5, "x"))

  println(hbalTrees(3, "x"))

  println(minHbalNodes(3))

  println(maxHbalHeight(4))

  println(hbalTreesWithNodes(15, 'x))

  println(Node('x', Node('x'), End).leafCount)

  println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList)

  println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2))

  println(completeBinaryTree(6, "x"))
}
