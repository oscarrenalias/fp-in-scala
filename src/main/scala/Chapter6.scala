trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & // `&` is bitwise AND
        ((1L << 48) - 1) // `<<` is left binary shift
      ((seed2 >>> 16).asInstanceOf[Int], // `>>>` is right binary shift with zero fill
        simple(seed2))
    }
  }

  // Exercise 1:
  def positiveInt(rng: RNG): (Int, RNG) = rng.nextInt match {
      case (v, next) if v == Int.MinValue => (1, next)
      case s@(v, next) => s
  }

  // Exercise 2:
  def nextDouble(rng: RNG): (Double, RNG) = {
    val r = RNG.positiveInt(rng)
    (Int.MaxValue.toDouble-r._1.toDouble, r._2)
  }

  // Exercise 3:
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val nextInt = rng.nextInt
    val nextDouble = RNG.nextDouble(nextInt._2)
    ((nextInt._1, nextDouble._1), nextDouble._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val nextDouble = RNG.nextDouble(rng)
    val nextInt = nextDouble._2.nextInt
    ((nextDouble._1, nextInt._1), nextInt._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val d1 = RNG.nextDouble(rng)
    val d2 = RNG.nextDouble(d1._2)
    val d3 = RNG.nextDouble(d2._2)
    ((d1._1, d2._1, d3._1), d3._2)
  }

  // Exercise 4: function to generate a list of random integers
  // Because we can, we'll do it with our own implementation of Streams
  // TODO: reimplement this using Scala's standard streams
  def ints(count: Int)(rng: RNG):List[Int] = {
    import Stream.Stream
    val (v, rng2) = rng.nextInt
    val s:Stream[(Int, RNG)] = Stream.unfold((v, rng2))(s => { val r=s._2.nextInt; Some((r,r))})
    s.take(count).toList.map(_._1)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  // EXERCISE 5: Use map to generate an Int between 0 and n, inclusive:
  def positiveMax(n: Int): Rand[Int] = RNG.map(RNG.positiveInt)(x => x / (Int.MaxValue / n))

  // EXERCISE 6: Use map to reimplement nextDouble in a more elegant way.
  def nextDouble_map: Rand[Double] = map(RNG.positiveInt)(Int.MaxValue.toDouble - _.toDouble)

  // EXERCISE 7: Unfortunately, map is not powerful enough to implement intDouble and doubleInt
  // from before. What we need is a new combinator map2, that can combine two RNG actions into one
  // using a binary rather than unary function. Write its implementation and then use it to
  // reimplement the intDouble and doubleInt functions.
  def map2[A,B, C](s1: Rand[A], s2: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rngA) = s1(rng)
    val (b, rngB) = s2(rngA)
    (f(a,b), rngB)
  }

  def intDouble_map2: Rand[(Int, Double)] =
    map2({ rng => rng.nextInt} /* _.nextInt */, nextDouble_map)((a,b) => ((a, b)))

  def doubleInt: Rand[(Double, Int)] = map2(nextDouble_map, _.nextInt)((a,b) => ((a,b)))

  // EXERCISE 8 (hard): If we can combine two RNG transitions, we should be able to combine
  // a whole list of them. Implement sequence, for combining a List of transitions into a
  // single transition. Use it to reimplement the ints function you wrote before. For the latter,
  // you can use the standard library function List.fill(n)(x) to make a list with x repeated n times.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f,acc) => map2(f, acc)((a,b) => a :: b))

  def nextInt: Rand[Int] = rng => rng.nextInt
  def ints_sequence(n: Int): Rand[List[Int]] = sequence(scala.collection.immutable.List.fill(n)(nextInt))

  // EXERCISE 9: Implement flatMap, then use it to reimplement positiveInt.
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def positiveInt_flatMap: Rand[Int] = flatMap(nextInt)(i => i match {
    case i if i == Int.MinValue => positiveInt_flatMap
    case i => unit(i.abs)
  })

  // EXERCISE 10: Reimplement map and map2 in terms of flatMap
  def map_flatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    val g: A => Rand[B] = a => unit(f(a))   // transform f:A=>B to f:A=>Rand[B]
    flatMap(s)(g)
  }

  def map2_flatMap[A,B, C](s1: Rand[A], s2: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(s1)(a => map(s2)(b => f(a,b)))
  }
}

case class State[S,+A](run: S => (A,S)) {
  def map[B](f: A=>B): State[S, B] = State(x => run(x) match {
    case (a,s) => (f(a), s)
  })

  def map2[B,C](s2: State[S,B])(f: (A,B) => C): State[S, C] = flatMap(a => s2.map(b => f(a,b)))

  def flatMap[B](f: A=>State[S,B]): State[S,B] = State(x => run(x) match {
    case (a,s) => f(a).run(s)
  })

  // EXERCISE 12: Come up with the signatures for getState and setState, then write their implementations.
  def getState[S]: State[S, S] = State(s => (s, s))

  def setState[S](s: S): State[S, Unit] = State(x => ((), s))
}

object State {
  // EXERCISE 11: Generalize the functions unit, map, map2, flatMap, and sequence. Add them
  // as methods on the State case class where possible. Otherwise you should put them
  // in a State companion object.

  def unit[S, A](a: A): State[S, A] = new State(x => (a, x))

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
}

// EXERCISE 13 (hard): To gain experience with the use of State, implement a simulation of a simple
// candy dispenser. The machine has two types of input: You can insert a coin, or you can turn the knob
// to dispense candy. It can be in one of two states: locked or unlocked. It also tracks how many
// candies are left and how many coins it contains.

/*
The rules of the machine are as follows:
- Inserting a coin into a locked machine will cause it to unlock if there is any candy left.
- Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
- Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
- A machine that is out of candy ignores all inputs.
 */

// Note: not finished

sealed trait Input
case object Coin extends Input

case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int) {
  // The method simulateMachine should operate the machine based on the list of inputs and return
  // the number of coins that the machine accepted during the simulation.

  def run(i: Input): State[Machine, Int] = (i, this) match {
    case (_, s@Machine(_, 0, _)) => State(x=>(coins, s))    // no candy - ignore all input
    case (Coin, s@Machine(false, _, _)) => State(x=>(coins, s))   // insert coin in unlocked machine - does nothing
    case (Turn, s@Machine(true, _, _)) => State(x=>(coins, s)) // turn a locked machine - does nothing
    case (Coin, Machine(true, candies, _)) => State(x=>(coins+1, Machine(false, candies, coins+1))) // insert coin if locked -> unlock
    case (Turn, Machine(false, candies, coins)) => State(x=>(coins, Machine(true, candies-1, coins)))  // not locked; give candy and lock
  }
}

case object Machine {
  def t(inputs: List[Input]): List[Machine => Machine] = {
    inputs.map(i => (s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    })
  }

  def simulateMachine(inputs: List[Input]): State[Machine, Int] = {
    inputs.map(i => (s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    })

    State.unit[Machine, Int](0)
  }
}