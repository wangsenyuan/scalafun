package s99.mtree

/**
 * Created by senyuanwang on 14/12/6.
 */
class MTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())

  def nodeCount: Int = children.foldLeft(1)((z, c) => z + c.nodeCount)

  def internalPathLength: Int = {
    children.foldLeft(0)((r, c) => r + c.nodeCount + c.internalPathLength)
  }

  def postorder: List[T] = children.flatMap(_.postorder) :+ value

  def lispyTree: String = children match {
    case Nil => value.toString
    case list => "(" + value.toString + " " + children.map(_.lispyTree).mkString(" ") + ")"
  }

  override def toString =
    children.foldLeft(value.toString)(_ + _) + "^"
}

object MTree {
  def apply[T](value: T) = new MTree(value, List())

  def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)

  def fromLispyString(s: String): MTree[String] = {
    def setNesting(nesting: Int, c: Char): Int = c match {
      case '(' => nesting + 1
      case ')' => nesting - 1
      case _ => nesting
    }
    def nextSpace(pos: Int, nesting: Int): Int =
      if ((s(pos) == ' ' || s(pos) == ')') && nesting == 0) pos
      else nextSpace(pos + 1, setNesting(nesting, s(pos)))
    def nextNonSpace(pos: Int): Int =
      if (s(pos) == ' ') nextNonSpace(pos + 1)
      else pos
    def listSubstrings(pos: Int): List[String] =
      if (pos > s.length || s(pos) == ')') Nil
      else {
        val end = nextSpace(pos, 0)
        s.substring(pos, end) :: (if (s(end) == ')') Nil else listSubstrings(nextNonSpace(end)))
      }
    if (s(0) != '(') MTree(s)
    else {
      val vEnd = nextSpace(1, 0)
      MTree(s.substring(1, vEnd), listSubstrings(nextNonSpace(vEnd)).map(fromLispyString(_)))
    }
  }

  implicit def string2MTree(s: String): MTree[Char] = {
    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    MTree(s(0), splitChildStrings(1).map(string2MTree(_)))
  }
}