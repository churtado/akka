package exercises

abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B>:A](element: B): MyList[B]
  def printElements: String
  // polymorphic call
  override def toString: String = "[" + printElements + "]"

  // higher order functions
  def map[B](transformer: A => B): MyList[B]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]

  def foreach(x:A => Unit): Unit  //[1,2,3].foreach(x => println(x))
  def sort( f:(A, A) => Int): MyList[A] // [1,2,3].sort((x,y)=> y-x) = [3,2,1] the function returns -1 if left term
  def zipWith[B](list: MyList[A], f:(A,A) => B): MyList[B]

  def ++[B >: A](list: MyList[B]) : MyList[B]
}

case object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): MyList[B] = new Cons(element, Empty)
  def printElements = ""

  def map[B](transformer: Nothing => B): MyList[B] = Empty
  def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = Empty
  def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty

  def foreach(x:Nothing => Unit): Unit = Unit
  def sort( f:(Nothing, Nothing) => Int): MyList[Nothing] = Empty

  def ++[B >: Nothing](list: MyList[B]) : MyList[B] = list
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyList[B] = new Cons(element, this)
  def printElements: String =
    if(t.isEmpty) "" + h
    else h + " " + t.printElements

  def filter(predicate: (A) => Boolean): MyList[A] =
    if(predicate(head)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)

  def map[B](transformer: A => B): MyList[B] =
    new Cons(transformer(h), t.map(transformer))
  /*
    [1,2] ++ [3,4,5]
    = new Cons(1, [2] ++ [3,4,5])
    = new Cons(1, new Cons(2, Empty ++ [3,4,5]))
    = new Cons(1, new Cons(2, new Cons(Empty, 3 ++ [4,5])))
      ...
    = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5)))))
   */
  def ++[B >: A](list: MyList[B]) : MyList[B] = new Cons(h, t ++ list)

  def flatMap[B](transformer: A => MyList[B]): MyList[B] = {
    transformer(h) ++ t.flatMap(transformer)
  }

  def concatenator: (String, String) => String = new Function2[String, String, String] {
    override def apply(v1: String, v2: String): String = {
      return v1 + v2
    }
  }

  def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }

  /*
    this one was super tricky.
    First, get the tail by calling sort on the tail and passing it f, that's the recursive call
    then call insert to insert the head

    insert itself is peculiar. It does sorting on insert. The function is defined inside the sort
    function. If you're inserting in an empty list, the element you're inserting becomes the head
    If the list isn't empty, check that the element you want to insert precedes the head of the list
    If that's true, insert the element as the head. If not, recursively call insert on a Cons() call,
    where the head of the list stays the head. Deceptively simple.
 */
  def sort(compare:(A, A) => Int): MyList[A] = {

    def insert(x:A , sortedList: MyList[A]): MyList[A] =
      if(sortedList isEmpty) new Cons(h, Empty)
      else if(compare(x, sortedList.head) <= 0) new Cons(x, sortedList)
      else new Cons(sortedList.head, insert(x, sortedList.tail))

    val sortedTail = t.sort(compare)
    insert(h, sortedTail)
  }

}

object ListTest extends App {
  val listOfInts: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val cloneListofInts: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))

  println("testing foreach")
  listOfInts.foreach(x => println(x))

  val listOfStrings: MyList[String] = new Cons("hello ", new Cons(" baby", new Cons(" boy", Empty)))
  println(listOfInts.tail.head)
  println(listOfInts.add(4).head)
  println(listOfInts isEmpty)
  println(listOfInts.toString)
  println(listOfStrings.toString)

  println(listOfInts.map(_ * 2).toString)

  println(listOfInts.filter(_ % 2 == 0))

  def superAdder: Function[Int, (Int) => Int] = new ((Int) => ((Int) => Int)) {
    override def apply(x: Int): (Int) => Int = {
      return new ((Int) => Int) {
        override def apply(y: Int): Int = x + y
      }
    }
  }

  val adder3 = superAdder(3)
  println(superAdder(3)(4)) // currying

  /*
  foreach method A => Unit
  [1,2,3].foreach(x=> println(x)
   */
}
