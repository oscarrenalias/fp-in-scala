package object Stream {

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty

    // EXERCISE 1: Write a function to convert a Stream to a List, which will
    // force its evaluation and let us look at it in the REPL. You can convert to the
    // regular List type in the standard library.
    def toList: List[A] = uncons match {
      case Some((head, tail)) => head :: tail.toList
      case _ => List()
    }

    // EXERCISE 2: Write a function take for returning the first n elements of a Stream.
    def take(n: Int): Stream[A] = {
      // my initial version:
      //
      /*def innerTake(n:Int, s:Stream[A], r:Stream[A]): Stream[A] = (n, s.uncons) match {
        case (0, _) => r
        case (n, Some((head, tail))) if s.isEmpty == false => innerTake(n-1, tail, Stream.cons(head, r))
        case _ => r
      }
      innerTake(n, this, Stream.apply[A]())*/

      // better version
      uncons match {
        case Some((head, tail)) if n > 0 => Stream.cons[A](head, tail.take(n-1))
        case _ => Stream[A]()
      }
    }

    // EXERCISE 3: Write the function takeWhile for returning all starting elements of a Stream
    // that match the given predicate.
    def takeWhile(p: A => Boolean): Stream[A] = uncons match {
      case Some((head, tail)) if p(head) => Stream.cons(head, tail.takeWhile(p))
      case _ => Stream()
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

    // EXERCISE 4: Implement forAll, which checks that all elements in the Stream match a
    // given predicate. Your implementation should terminate the traversal as soon as it
    // encounters a non-matching value.
    def forAll(p: A => Boolean): Boolean = {
      def lazyForAll[A](s:Stream[A], acc:Boolean)(f: (A, => Boolean) => Boolean): Boolean = s.uncons match {
        case Some((head, tail)) => lazyForAll(tail, f(head, acc))(f)
        case None => acc
      }
      lazyForAll(this, true)((a,b) => p(a) && b)
    }

    def forAll_foldRight(f: A => Boolean): Boolean = foldRight(true)((a,b) => f(a) && b)

    // EXERCISE 5: Use foldRight to implement takeWhile. This will construct a stream incrementally,
    // and only if the values in the result are demanded by some other expression.
    def takeWhile_foldRight(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])((a,b) => if(p(a)) Stream.cons(a, b); else Stream.empty)

    // EXERCISE 6: Implement map, filter, append, and flatMap using foldRight.
    def map[B](f: => A => B): Stream[B] =
      foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a), b))

    def filter(f: => A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])((a,b) => if(f(a)) Stream.cons(a, b); else b)

    def append[B>:A](s: Stream[B]): Stream[B] =
      foldRight(s)((a,b) => Stream.cons(a, b))

    def flatMap[B>:A](f: => A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B])((a,b) => b.append(f(a)))

    // EXERCISE 12: Use unfold to implement map, take, takeWhile, zip (as
    // in chapter 3), and zipAll. The zipAll function should continue the traversal as
    // long as either stream has more elements — it uses Option to indicate whether
    // each stream has been exhausted.
    def take_unfold(n: Int): Stream[A] = Stream.unfold((this, n))(p => p match {
      case (s,n) if n > 0 => s.uncons.map { case (head, tail) => (head, (tail, n-1)) }
      case _ => None
    })

    // this is getting tricky for me - didn't get this one
    def takeWhile_unfold(f: A => Boolean): Stream[A] = Stream.unfold(this)(s => s.uncons match {
      case Some((head, tail)) if(f(head)) => Some((head, tail))     // alternative: case s@Some(head, tail) if(...) => s
      case _ => None
    })

    // but I did do these right :)
    def map_unfold[B](f: A => B):Stream[B] = Stream.unfold(this)(s => s.uncons match {
      case Some((head, tail)) => Some((f(head), tail))
      case _ => None
    })

    def zip[B](s2: Stream[B]): Stream[(A,B)] = Stream.unfold((this,s2))(s => (s._1.uncons, s._2.uncons) match {
      case(Some((h1, t1)), Some((h2, t2))) => Some(((h1, h2), (t1, t2)))
      case _ => None
    })

    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold((this,s2))(s => (s._1.uncons, s._2.uncons) match {
      case(Some((h1, t1)), Some((h2, t2))) => Some(((Some(h1), Some(h2)), (t1, t2)))
      case(Some((h1, t1)), _) => Some(((Some(h1), None), (t1, Stream.empty[B])))  // second stream is over, but continue
      case(_, Some((h2, t2))) => Some((None, Some(h2)), (Stream.empty[A], t2))    // first stream is over, but continue
      case _ => None
    })

    // EXERCISE 13 (hard): implement startsWith using functions you've
    // written. It should check if one is a prefix of another. Stream For instance,
    // Stream(1,2,3) starsWith Stream(1,2) would be true.

    // naive implementation that does not work for this case: Stream(1,2,3).startsWith_naive(Stream(1,2,3,4)) - should be false
    def startsWith_naive[B>:A](s2: Stream[B]): Boolean = zip(s2).forAll(a => a._1 == a._2)

    // This one seems to cover all cases but it may not be the most efficient of all, since it basically
    // zipsAll both sequences into one and then checks all pairs
    def startsWith[B>:A](s2:  Stream[B]): Boolean = zipAll(s2).forAll(a => a match {
      case (Some(v1), Some(v2)) if v1 == v2 => true
      case (Some(v1), Some(v2)) if v1 != v2 => false
      case (Some(v1), None) => true
      case (None, _) => false
      case _ => false
    })

    // EXERCISE 14: implement tails using unfold. For a given Stream,
    // tails returns the Stream of suffixes of the input sequence, starting with the
    // original Stream. So, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3),
    // Stream.empty).
    def tails: Stream[Stream[A]] = Stream.unfold(this)(s => s.uncons match {
      case Some((head, tails)) => Some((s, tails))
      case None => None
    })

    // Exercise 15 (hard): generalize tails to the function scanRight, which is like a
    // foldRight that returns a Stream of intermediate results
    // Example: Stream(1,2,3).scanRight(0)(_+_) = Stream(6, 5, 3, 0)

    // I was not able to figure this one out, so this is the solution from the book
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z)))((a,p) => {
        val b2 = f(a,p._1)
        (b2, Stream.cons(b2,p._2))
      }) _2
  }

  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] { def uncons = None }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    // EXERCISE 7: Generalize ones slightly to the function constant which returns
    // an infinite Stream of a given value.
    def constant[A](a: A): Stream[A] = Stream.cons(a, Stream.constant(a))

    // EXERCISE 8: Write a function that generates an infinite stream of integers,
    // starting from n, then n + 1, n + 2, etc
    def from(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n+1))

    // EXERCISE 9: Write a function fibs that generates the infinite stream
    // of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
    def fibs: Stream[BigInt] = {
      def innerFibs(v1: BigInt, v2: BigInt): Stream[BigInt] = Stream.cons(v1, innerFibs(v2, v1 + v2))
      innerFibs(0,1)
    }

    // EXERCISE 10: We can write a more general stream building function. It takes an
    // initial state, and a function for producing both the next state and the next value
    // in the generated stream. It is usually called unfold:
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = new Stream[A] {
      def uncons: Option[(A, Stream[A])] =
        f(z).flatMap({ x => Some(x._1, unfold(x._2)(f)) })
    }

    // official solution
    def unfold_official[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((head, state)) => Stream.cons(head, unfold(state)(f))
      case None => Stream.empty[A]
    }

    // EXERCISE 11: Write fibs, from, constant, and ones in terms of unfold.
    def ones_unfold = unfold(1)(s => Some((1, 1)))
    def constant_unfold[A](a: A) = unfold(a)(s => Some((a, a)))
    def from_unfold(n: Int): Stream[Int] = unfold(n)(s => Some((s + 1, s + 1)))
    // note: I did not get this one right
    def fibs_unfold:Stream[BigInt] = Stream.cons(0, Stream.unfold((0,1)) { case (f0,f1) => Some((f1,(f1,f0+f1))) })
  }

  // test data
  val s1 = Stream(1,2,3,4,5)
  s1.toList   // List(1,2,3,4,5)
  s1.take(3)    // Stream(1,2,3)
  s1.takeWhile(_ < 4)   // Stream(1,2,3)
  s1.forAll(_ <= 5)    // true
  s1.forAll(_ % 2 == 0) // false
  s1.forAll_foldRight(_ % 2 == 0) // false
  s1.takeWhile_foldRight(_ < 4)   // Stream(1,2,3)

  val s2 = s1.map(_ * 2)
  val sFilter = s1.filter(_ % 2 == 0)
  val s3 = Stream(1,2,3,4).append(Stream(5,6,7,8))
  val s4 = s1.flatMap(x => Stream(x*2))

  val s10 = Stream.constant(10)
  val sFrom10 = Stream.from(10)

  // unfold-based implementations
  val sConstant = Stream.constant_unfold(1)
  val sFrom = Stream.from_unfold(10)
  val sTake = s1.take_unfold(3)
  Stream(1,2,3,4,5).zip(Stream("one", "two", "three", "four", "five"))
  Stream(1,2,3).zipAll(Stream("one", "two", "three", "four", "five"))

  Stream(1,2,3,4).startsWith(Stream(1))   // true?
  Stream(1,2,3,4).startsWith(Stream(1,2))   // true?
  Stream(1,2,4,5).startsWith(Stream(4,5))   // false?

  Stream(1,2,3).tails   // Stream(Stream(1,2,3), Stream(2,3), Stream(3))
}