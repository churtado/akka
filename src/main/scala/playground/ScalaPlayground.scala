package playground

object ScalaPlayground extends App {
  // Define currying function
  def add(x: Int, y: Int) = x + y
  println(add(20, 19))

  def add2(x: Int) = (y: Int) => x + y
  val add4 = add2(4)

  println(add4(1)) // partial application
  println(add2(7)(1))


}
