package s99.mtree

/**
 * Created by senyuanwang on 14/12/6.
 */
object MTreeApp extends App {

  import s99.mtree.MTree._

  println(MTree('a', List(MTree('f'))).nodeCount)

  println(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString)

  println(MTree.string2MTree("afg^^c^bd^e^^").toString)

  println("afg^^c^bd^e^^".internalPathLength)

  println("afg^^c^bd^e^^".postorder)

  println("afg^^c^bd^e^^".lispyTree)

  println(MTree.fromLispyString("(a (f g) c (b d e))").lispyTree)
}
