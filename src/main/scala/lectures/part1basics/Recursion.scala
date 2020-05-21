package lectures.part1basics

import scala.annotation.tailrec

object Recursion extends App {

  def factorial(n: Int): Int ={
    if (n==0 || n==1){
      1
    } else {
      println("calculating. Must get  " + (n-1))
      val result = n * factorial(n-1)
      println("obtained intermediate result")
      result
    }
  }

  // this works
  //println(factorial(10))

  // this causes stack overflow. But returned 0. wtf???? I think because there's lots of RAM so more stack space, and more stack frames
  // it goes to zero because Int doesn't have the magnitude for big numbers. So we rewrite with BigInt
  // println(factorial(1000))
  //

  def factorial2(n: BigInt): BigInt = {
    def helper(x: BigInt, accumulator: BigInt): BigInt = {
      if(x<=1) accumulator
      else helper(x-1, x*accumulator) //TAIL RECURSION
    }

    helper(n, 1)
  }

  /*
    factorial2(10)??
    helper(10, 1)
    helper(9, 10*1)
    helper(8, 9*10*1)
    helper(7, 8*9*10*1)
      .....
    helper(1, 2*3*4*...*8*9*10*1) -> return accumulator
    2*...*10*1
   */

  //println(factorial2(5000))

  /*
  so why did factorial2 work and factorial didn't?
  factorial needs to store an intermediate result to multiply with the recursive call (line 10: n * fact(n-1))
  whereas helper defines the return statement as a fully recursive call with no intermediate result.
  This means that no stack frame is needed to store intermediate results, and therefore you will not get
  a stack overflow error. The compiler will know to replace each call of helper with the proper parameters until
  getting the ultimate last call you see on line 40. That's called TAIL RECURSION
  Use recursive call as last expression!
   */

  // when you need loops use tail recursion
  @tailrec
  def concatenate(s:String, n: Int, accumulator: String): String = {
    if(n<=1) s+accumulator
    else concatenate(s, n-1, accumulator + s)
  }

  //println(concatenate("hello",3,""))

  def fibonacci(n: Int): Int= {
    @tailrec
    def fibhelper(n: Int, accumulator: Int): Int = {
      if (n<=1) 1 + accumulator
      else if (n==2) 3 + accumulator
      else fibhelper(n-1, n+accumulator)
    }
    fibhelper(n, 0)
  }
  println(fibonacci(0))
  println(fibonacci(1))
  println(fibonacci(2))
  println(fibonacci(3))


}
