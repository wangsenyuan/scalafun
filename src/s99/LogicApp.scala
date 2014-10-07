package s99

/**
 * Created by senyuanwang on 14-10-7.
 */
object LogicApp extends App {

  import s99.LogicOp._

  table2((a: Boolean, b: Boolean) => and(a, or(a, b)))

  table2((a: Boolean, b: Boolean) => a and (a or not(b)))
}
