package s99

/**
 * Created by senyuanwang on 14-10-7.
 */
object TreeApp extends App {

  import s99.tree.Tree._

  println(cBalanced(4, 'x))

  println(fromList(List(3, 2, 5, 7, 1)))

  println(fromList(List(3, 2, 5, 7, 1)).isSymmetric)

  println(fromList(List(3, 2, 5, 7, 4)).isSymmetric)

  println(symmetricBalancedTrees(5, "x"))

  println(hbalTrees(3, "x"))
}
