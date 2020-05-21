package lectures.part1basics

object CBNvCBV extends App {

  def calledByValue(x: Long): Unit = {
    println("by value: " + x)
    println("by value: " + x)
  }

  def calledByName(x: => Long): Unit = {
    println("by name: " + x)
    println("by name: " + x)
  }

  println(calledByValue(System.nanoTime()))
  println(calledByName(System.nanoTime()))

  /*
  in call by value, the parameter is evaluated once before the function runs. Hence the value doesn't change
  in call by value, the actual expression is passed to the method, and evaluated every time.

  basically, evaluate first and only once, then call method is by value
             evaluate every time and pass the actual expression into the function, call by name

  expression is passed literally in pass by name!

  good to know for lazy functions!
   */

  def infinite(): Int = 1 + infinite() // causing a stack overflow!
  def printFirst(x: Int, y: => Int) = println(x)

  // this will crash because infinite needs to be evaluated first!
  //println(printFirst(infinite(),1))

  // this won't crash because infinite is never evaluated!
  println(printFirst(1, infinite()))
}
