package s99

/**
 * Created by senyuanwang on 14-10-4.
 */
object ListApp extends App {

  import s99.ListOp._

  println(last(List(1, 1, 2, 3, 5, 8)))

  println(penultimate(List(1, 1, 2, 3, 5, 8)))

  println(nth(2, List(1, 1, 2, 3, 5, 8)))

  println(length(List(1, 1, 2, 3, 5, 8)))

  println(reverse(List(1, 1, 2, 3, 5, 8)))

  println(isPalindrome(List(1, 2, 3, 2, 1)))

  println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))

  println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
}
