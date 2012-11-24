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
}

val r1 = RNG.simple(10L)
RNG.positiveInt(r1)   // (some integer, rng)