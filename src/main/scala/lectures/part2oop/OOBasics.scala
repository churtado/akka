package lectures.part2oop

object OOBasics extends App {
  val person = new Person("bob", 30)
  println(person.age)
  println(person.x)
  person.greet("Carlos")
  person.greet
}

// constructor
class Person(name: String, val age: Int) { // class parameters are not fields! have to add val, for example to age. person.name would give an error
  val x = 2

  println(1+3) // gets printed first!

  def greet(name: String): Unit = println(s"Hi $name, from ${this.name}")

  def greet(): Unit = println(s"Hi, I'm $name")

  // multiple constructors
  def this(name: String) = this(name,0)
}
