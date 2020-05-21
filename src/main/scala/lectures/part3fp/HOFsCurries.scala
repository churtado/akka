package lectures.part3fp

object HOFsCurries extends App {

  // function that applies a function n times over a value x
  // nTimes(f,n,x)
  // nTimes(f,3,x) = f(f(f(x)))

  def nTimes(f:Int => Int, n: Int, x: Int) : Int = {
    if (n<=0) x
    else nTimes(f, n-1, f(x))
  }

  val plusOne = (x: Int) => x + 1

  print(nTimes(plusOne, 3, 1))

  // ntb(f, n) = (x) => f(f(f...f(x)...)
  // ntb(f, 3) = (x) => f(f(f(x)))
  def nTimesBetter(f: Int => Int, n: Int): (Int => Int) = {
    if(n <= 0) (x:Int) => x // return the very thing you're evaluating!!! Since everything evals to a number, this will return the number
    else (x: Int) => nTimesBetter(f, n-1)(f(x))
  }

  // (1) ntb(plusone, 3) -> (x) => ntb(plusone, 2) (plusone(x))
  // (2) ntb(plusone, 2) -> (y) => ntb(plusone, 1) (plusone(y))
  // (3) ntb(plusone, 1) -> (z) => ntb(plusone, 0) (plusone(z))
  // (4) ntb(plusone, 0) -> (w) => w

  // (5) ntb(plusone, 3) -> (x) =>  (y) => (ntb(plusone, 1) (plusone(y))) (plusone(x))
  // (6) ntb(plusone, 3) -> (x) =>  (y) => (   ((z) => ntb(plusone, 0) (plusone(z))) (plusone(y))    ) (plusone(x))
  // (7) ntb(plusone, 3) -> (x) =>  (y) => (   ((z) => id       (plusone(z))) (plusone(y))    ) (plusone(x))

  // let adder3 = ntb(plusone, 3)
  // thus, adder3(1) ->
  // (1) =>  (y) => (   ((z) => id       (plusone(z))) (plusone(y))    ) (plusone(1))
  // (1) =>  (y) => (   ((z) => id       (plusone(z))) (plusone(y))    ) (2)
  // (1) =>  (2) => (   ((z) => id       (plusone(z))) (plusone(2))    ) (2)
  // (1) =>  (2) => (   ((z) => id       (plusone(z))) (3)    ) (2)
  // (1) =>  (2) => (   ((3) => id       (plusone(z))) (3)    ) (2)
  // (1) =>  (2) => (   ((3) => id       (plusone(3))) (3)    ) (2)
  // (1) =>  (2) => (   ((3) => id       (4)) (3)    ) (2)
  // id(4) = 4

}
