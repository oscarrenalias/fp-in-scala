package object Ep2 {
  def abs(n: Int): Int = if (n < 0) -n else n

  def absolute(f: Int => Int): Int => Int = {   // Exercise 2
    n => abs(f(n))
  }

  def absoluteT[A](f: A => Int): A => Int = { // Exercise 3
    n => abs(f(n))
  }

  type Pred[A] = A => Boolean
  def divisibleBy(k: Int): Pred[Int] = {  // Exercise 4
    // whether a given number is divisible by 'k'
    x => (k % x) == 0
  }

  def divisibleBy3And5: Pred[Int] = {
    // without the 'lift' method:
    //x => ((x % 3) == 0) && ((x % 5) == 0)

    // using 'lift' as the combinator
    lift(
      (x,y) => x && y,
      x => x % 3 == 0,
      x => x % 5 == 0
    )
  }

  def divisibleBy3Or5: Pred[Int] = {
    // without the 'lift' method:
    //x => ((x % 3) == 0) || ((x % 5) == 0)
    // using 'lift' as the combinator
    lift(
      (x,y) => x || y,
      x => x % 3 == 0,
      x => x % 5 == 0
    )
  }

  def lift[A](f: (Boolean, Boolean) => Boolean, g: Pred[A], h: Pred[A]): Pred[A] = {
    n => f(g(n), h(n))
  }

  def runMe = {
    val test: Int => Int = x => x*2
    val newAbs = absolute(test)
    println(newAbs(-123))   // prints "246"
    println(newAbs(123))    // prints "246"

    // polymorphic absolute method
    val test2: String => Int = s => s.length
    val strAbs = absoluteT(test2)
    println(strAbs("abc"))    // prints "3"
    println(strAbs("defa"))    // prints "4"

    // divisible by 'k'
    val divisibleBy2 = divisibleBy(2)
    println(divisibleBy2(2))  // true
    println(divisibleBy2(5))  // false
  }
}

object Main {
  def main(args: Array[String]) = {

  }

}
