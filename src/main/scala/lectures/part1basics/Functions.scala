package lectures.part1basics

object Functions extends App {
  def aFunction(a: String, b:Int) = {
    a + " " + b
  }

  println(aFunction("hello",3))

  def parameterlessFunction(): Int = 42
  println(parameterlessFunction)

  def aRepeatedFunction(aString: String, n:Int): String = {
    if (n ==1) aString
    else aString + aRepeatedFunction(aString, n-1)
  }

  println(aRepeatedFunction("hello", 3))

  // when you need loops, use recursion!!!!

  def sideEffectsFunction(aString: String): Unit = {
    println("hello")
  }

  def aBigFunction(a: Int) = {
    def aSmallerFunction(b: Int): Int ={
      1
    }
    aSmallerFunction(1)
  }

  def factorial(n: Int): Int ={
    if (n==0 || n==1){
      1
    } else {
      n * factorial(n-1)
    }
  }

  println(factorial(3))


}
