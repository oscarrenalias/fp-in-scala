//package object Stream {

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
//}