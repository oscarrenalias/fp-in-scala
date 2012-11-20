package object Chapter4 {

	// Exercise 1: implement the functions below:		
	trait Option[+A] {
	  def map[B](f: A => B): Option[B] = this match {
	  	case Some(v) => Some(f(v))
	  	case _ => None
	  }
	  
	  def flatMap[B](f: A => Option[B]): Option[B] = this match {
	  	case Some(v) => f(v)
	  	case _ => None
	  }
	  
	  def getOrElse[B>:A](default: => B): B = this match {
	  	case Some(v) => v
	  	case _ => default
	  }

	  def orElse[B>:A](ob: Option[B]): Option[B] = this match {
	  	case Some(v) => this
	  	case _ => ob
	  }
	  
	  def filter(f: A => Boolean): Option[A] = this match {
	  	case Some(v) if(f(v)) => this
	  	case _ => None
	  }
	}	
	case class Some[+A](get: A) extends Option[A]
	case object None extends Option[Nothing]

	val o1 = Some(10)
	val o2 = Some(true)
	val o3 = None
	val o4 = Some("Hello, world")
	val o5 = Some(44)

	o1.map(_ * 2)
	o3.orElse(Some("It was empty"))
	o3.getOrElse("It was empty")
	o1.flatMap(v=>Some(v*100))

	// Exercise 2: Implement the variance function (if the mean is m, variance is 
	// the mean of math.pow(x - m, 2), see definition) in terms of mean and flatMap
	def variance(xs: Seq[Double]): Option[Double] = {
		// note: I'm not sure I got this exercise right...
		def mean(l:Seq[Double]) = (l.foldLeft[Double](0)((x, y) => x+y)) / l.length
		Some(mean(xs.map(x=>math.pow(x - 2, 2))))
	}

	// EXERCISE 3: Write a generic function map2, that combines two 
	// Options into one using a binary function. If either Option value is None, 
	// then the return value is too. Here is its signature:
	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
		(a,b) match {
			case (None, _) | (_, None) => None
			case (Some(v1), Some(v2)) => Some(f(v1,v2))
		}
	}

	// Better implementation - I must be stupid not to have seen this one :)
	def map2_better[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
		for {
			a1 <- a
			b1 <- b
		} yield(f(a,b))
	}

	map2(o1, o5)((x,y) => x*y)
	map2_better(o1, o5)((x,y) => x*y)

	// EXERCISE 4: Re-implement bothMatch above in terms of this new function, to the extent possible.
	import java.util.regex._
	def pattern(s: String): Option[Pattern] = try {
    		Some(Pattern.compile(s))
  		} catch {
    		case e: PatternSyntaxException => None
		}

	def mkMatcher(pat: String): Option[String => Boolean] = 
		pattern(pat) map (p => (s: String) => p.matcher(s).matches)

	def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] = for {
    	f <- mkMatcher(pat)
    	g <- mkMatcher(pat2)
  	} yield f(s) && g(s)

  	// My implementation:
	def bothMatch_new(pat1: String, pat2: String, s: String): Option[Boolean] = {
		map2(mkMatcher(pat1), mkMatcher(pat2))((res1, res2) => res1(s) && res2(s))
	}

	val p1="[a-zA-Z]*"
	val p2="[a-z]*"
	bothMatch(p1, p2, "aaa")		// true
	bothMatch_new(p1, p2, "aaa")	// true
	bothMatch_new(p1, p2, "123")	// false

	// EXERCISE 5: Write a function sequence, that combines a list of Options into one option containing a 
	// list of all the Some values in the original list. If the original list contains None even once, 
	// the result of the function should be None, otherwise the result should be Some with a list of all 
	// the values.
	def sequence[A](a: List[Option[A]]): Option[List[A]] = (a.foldRight[Option[List[A]]](Some(Nil))((x,y) => x match {
		case None => None
		case Some(x) => y.map(l => x :: l)
	}))	
	// Seems to work, though I'm still not sure if the one above is as efficient as it could be...
	
	// better version using map2
	def sequence_better[A](a: List[Option[A]]): Option[List[A]] = 
		a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

	val ol1 = List(Some(1), Some(2), Some(3), Some(4))
	val ol2 = List(Some(1), Some(2), None, Some(3), Some(4))
	sequence(ol1)		// Some(List(1,2,3,4))
	sequence(ol2)		// None, since there's one None in the list
	sequence_better(ol2)		// None, since there's one None in the list

	// EXERCISE 6: Implement the function traverse. It is straightforward to do using map and sequence, 
	// but try for a more efficient implementation that only looks at the list once. In fact, 
	// implement sequence in terms of traverse.
	def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a.map(x => f(x)))	// trivial :)

	def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
		a.foldRight[Option[List[B]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))
}