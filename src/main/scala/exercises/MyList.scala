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
  def zipWith[B,C](list:MyList[B], f:(A,B) => C): MyList[C] //[1,2,3].zipWith([4,5,6], x*y) => [1*4,2*5,3*6]
  def fold[B](start:B)(operator: (B,A) => B): B

  def ++[B >: A](list: MyList[B]) : MyList[B]
}

case object Empty extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](element: B): MyList[B] = new Cons(element, Empty)
  override def printElements = ""

  override def map[B](transformer: Nothing => B): MyList[B] = Empty
  override def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = Empty
  override def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty

  override def foreach(x:Nothing => Unit): Unit = Unit

  override def sort( f:(Nothing, Nothing) => Int): MyList[Nothing] = Empty

  override def zipWith[B, C](list: MyList[B], f: (Nothing, B) => C): MyList[C] =
    if(!list.isEmpty) throw new RuntimeException("lists don't have the same length")
    else Empty

  override def fold[B](start: B)(operator:(B, Nothing) => B) = start

  override def ++[B >: Nothing](list: MyList[B]) : MyList[B] = list

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

  def concatenator: (String, String) => String = new ((String, String) => String) {
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

  override def zipWith[B, C](list: MyList[B], f: (A, B) => C): MyList[C] =
    if(list.isEmpty) throw new RuntimeException("lists don't have the same length")
    else new Cons(f(h, list.head), t.zipWith(list.tail,f))

  /*
  [1,2,3].fold(0)(+) =
  [2,3].fold(1+0) =
  [2,3].fold(1) =
  [3].fold(1+2) =
  [3].fold(3) =
  [].fold(3+3) =
  [].fold(6) =
  6
  ((([].fold(3+3))).fold(1+2)).fold(1+0)).fold(0)(+)
   */
  override def fold[B](start: B)(operator:(B,A)=>B): B =
    t.fold(operator(start,h)) (operator)

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

  /*
  1 + 2 = 3

   */
  def toCurry(f:(Int, Int) => Int): (Int=>Int=>Int)  = {
    x => y => f(x,y)
  }

  def fromCurry(f:(Int=>Int=>Int)) : (Int, Int) => Int = {
    (x, y) => f(x)(y)
  }

  def compose[A,B,T](f: A => B,g: T => A) : T => B =
    x => f(g(x))

  def andThen[A,B,T](f: A => T, g: T => B) : A => B =
    x => g(f(x))

}
