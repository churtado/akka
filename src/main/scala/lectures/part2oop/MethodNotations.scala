package lectures.part2oop

object MethodNotations extends App {

  class Person( val name: String, age: Int = 0, favoriteMovie: String = "") {
    def likes(movie: String): Boolean = movie == favoriteMovie
    def +(person: Person): String = s"${this.name} is hanging out with ${person.name}"
    def +(nickname: String): Person = new Person(this.name + "(" + nickname + ")")
    def unary_+(): Person = new Person(this.name, age + 1)
    def learns(subject: String): String = this.name + " learns " + subject
    def learnsSCala(): String = learns("Scala")
    def unary_! : String = s"$name, what the heck?"
    def isAlive: Boolean = true
    def apply(): String = s"Hi I'm $name"
    def apply(times: Int): String = s"$name watched $favoriteMovie $times times"
  }

  val mary = new Person(name = "Mary", favoriteMovie = "Inception")
  val bob = new Person("Bob", favoriteMovie = "Foo")
  println(mary likes "Inception")
  println(mary + bob) // all operators are methods
  println(!mary)
  println(mary isAlive)
  println(mary())
  println((+mary).name)
  println(mary learnsSCala)
  println(mary(2))

}
