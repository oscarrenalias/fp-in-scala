package object Chapter2 {
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

  // Exercise 7 - currying a given function
  def curry[A,B,C](f: (A,B) => C): A => B => C = {
    a => f(a, _)
  }

  // Exercise 8 - uncurrying a function
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a,b) => (f(a))(b)
  }

  // Exercise 9 - function composition
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  // this did not have an exercise number, but it's a polymorphic version of the lift function above
  def liftPoly[A,B,C,D](f: (B, C) => D)(g: A => B, h: A => C): A => D = {
    n => f(g(n), h(n))
  }

  // Exercise 10 - lift3
  def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B, h: A => C, i: A => D): A => E = {
    a => f(g(a), h(a), i(a))
  }

  // Exercise 11 - lift3 defined in terms of liftPoly
  def better_lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B, h: A => C, i: A => D): A => E = {
    // curry f so that we can use it as a 2 parameter function, rather than 3
    val f2: (B,C) => D => E = (b,c) => f(b, c, _)
    val liftedF2 = liftPoly(f2)(g, h)
    // the first part takes care of the D=>E so that we end up with A=>E
    a => liftedF2(a)(i(a))
  }

  // Exercise 12 - recursive Fibonacci vs. tail-recursive Fibonacci
  def recFib(n: Int): Int = n match {
    case 0 => n
    case 1 => n
    case _ => recFib(n-1) + recFib(n-2)
  }

  def tailrecFib(n:Int): Int = {
    def tailrecFibLoop(n: Int, a: Int, b: Int): Int= n match {
      case 0 => b
      case _ => tailrecFibLoop(n-1, a+b, a)
    }
    tailrecFibLoop(n, 1, 0)
  }

  // Exercise 13 - iterateWhile, a function that iteratively applies a given function
  // f while the predicate p holds, returning the first value that doesn't match
  // (discarding values that match),
  def sqrt(n: Double): Double = {
    def f(x: Double) = (x * x) - n
    iterateWhile(2.0)(x => x - f(x) / (2 * x),
      x => f(x).abs > 1e-14)
  }

  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A = {
    val r=f(a)
    p(r) match {
      case true => iterateWhile(r)(f, p)
      case false => r
    }
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

    // exercise 7 and 8
    val f: (Int, Int) => String = (x, y) => "Result is: " + (x*y)
    println(f(2,3))   // prints 6
    // should print 6
    val curriedF = curry(f)
    val curriedF_2 = curriedF(2)
    println(curriedF_2(3))

    val uncurriedF = uncurry(curriedF)
    println(uncurriedF(2,3))  // should print 6

    // exercise 9 - function composition
    val f1: Int => String = x => (x*2).toString
    val f2: String => Int = s => Integer.parseInt(s)
    val fCompose = compose(f2, f1)
    val fCompose2 = f2 compose f1 // using the standard Scala methods
    println(fCompose(3))  // should print 6
    println(fCompose2(3))  // should print 6

    // test polymorphic lift
    val liftedF = liftPoly[Int,Boolean,Boolean,Boolean]((x,y) => x && y)(x => x % 3 == 0, x => x % 5 == 0)
    println(liftedF(3))   // false
    println(liftedF(5))   // false
    println(liftedF(15))  // true
    // polymorphic lift with simulated box type
    case class Box(height: Double, width: Double)
    val divBox = liftPoly[Box, Double, Double, Double](_ / _)_
    val aspectRatio = divBox(_.height, _.width)
    println(aspectRatio(Box(1,2)))    // 0.5

    // lift3 and its better version, though I'm lacking some imagination here
    val f11: String => Int = s => s.length
    val f22: String => Int = s => s.length
    val f33: String => Int = s => s.length
    val fComb: (Int, Int, Int) => Int = (a,b,c) => a+b+c
    val f_lift3 = lift3(fComb)(f11, f22, f33)
    println(f_lift3("test"))   // prints 12
    val f_betterLift3 = better_lift3(fComb)(f11, f22, f33)
    println(f_betterLift3("test"))   // prints 12

    // exercise 12
    println(recFib(10))   // 55
    println(tailrecFib(10)) // 55

    // exercise 13
    println(sqrt(16))   // 4
  }
}
