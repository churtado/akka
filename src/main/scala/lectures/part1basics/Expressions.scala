package lectures.part1basics

object Expressions extends App {

  val x = 1 + 2 // expression
  println(x)
  println(2+3*4)

  println(1 == x)

  println(!(1==x))

  var avariable = 2 // a variable
  avariable += 3 // -=,+=, etc are all side effects, they work on variables

  // instructions vs expressions
  // instructions: tell the computer to do something e.g. println()
  // expression is something that has a value or type

  // example: if expression
  val aCondition = true
  val aConditionValue = if(aCondition) 5 else 3 // this is an if expression not an instruction

  println(if(aCondition) 5 else 3)

  // vars are frowned because they create side effects
  // iterate with an instruction: while loop
  var i=0
  while (i<30){
    println(i)
    i+=1
  }
  // this causes side effects to i. Avoid while loops as well

  // everything in Scala is an expression
  val aWeirdValue= (avariable = 3) // Unit === void
  println(aWeirdValue)

  // side effects are cause by whiles, println, reassigning. But the expressions return void or Unit
  // so think of instructions as expressions that return Unit

  // code blocks
  val aCodeBlock = {
    val y = 2
    val z = y + 1
    if (z>2) "hello" else "goodbye"
  }
  // code blocks return the value of the last expression, and state within block is isolated to the block



}
