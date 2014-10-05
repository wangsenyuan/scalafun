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

  println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))

  println(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  println(duplicate(List('a, 'b, 'c, 'c, 'd)))

  println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))

  println(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  println(removeAt(1, List('a, 'b, 'c, 'd)))

  println(insertAt('new, 1, List('a, 'b, 'c, 'd)))

  println(range(4, 9))

  println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))

  println(lotto(6, 49))

  println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))

  println(combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)))

  println(group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")))

  println(lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))

  println(lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))
}
