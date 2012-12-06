import java.util.concurrent.{TimeUnit, Callable, Future, ExecutorService}

type Par[A] = ExecutorService => Future[A]

case class SimpleFuture[A](a: A) extends Future[A] {
  def cancel(mayInterruptIfRunning: Boolean) = false
  def isCancelled = false
  def isDone = true
  def get = a
  def get(timeout: Long, unit: TimeUnit) = get
}

case object Par {
  // EXERCISE 3: Let's begin by implementing the functions of the API we've developed
  // so far. Now that we have a representation for Par, we should be able to fill these in.
  // Optional (hard): try to ensure your implementations respect the contract of the get
  // method on Future that accepts a timeout.
  def unit[A](a: A): Par[A] = es => SimpleFuture(a)

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call() = a(es).get
  })

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val vA = Par.run(es)(a)
    val vB = Par.run(es)(b)
    SimpleFuture(f(vA.get, vB.get))
  }

  // EXERCISE 4: This API already enables a rich set of operations. Here's a simple example: using async, write a
  // function to convert any function A => B to one that evaluates its result asynchronously:
  def asyncF[A,B](f: A => B): A => Par[B] = a => fork(unit(f(a)))

  // EXERCISE 5 (optional): Implement product and map as primitives, then define map2 in terms of them.
  def product[A,B](fa: Par[A], fb: Par[B]): Par[(A,B)] = es => {
    val vA = Par.run(es)(fa)
    val vB = Par.run(es)(fb)
    SimpleFuture((vA.get, vB.get))
  }

  def map[A,B](fa: Par[A])(f: A => B): Par[B] = es => {
    SimpleFuture(f((Par.run(es)(fa)).get))
  }

  def map2_new[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    map(product(a,b)){ case (a,b) => f(a,b) }
  }

  // EXERCISE 6: Note that we could always just write parMap as a new primitive. See if you can
  // implement it this way. Remember that Par[A] is simply an alias for ExecutorService => Future[A].
  // Here is the signature for parMap:
  def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = es => {
    es.submit(new Callable[List[B]] {
      def call() = l.map(x => f(x))
    })
  }

  // Better implementation from the solution:
  def parMap_better[A,B](l: List[A])(f: A => B): Par[List[B]] = es => {
    val fs: List[Future[B]] = l map (a => asyncF(f)(a)(es))
    SimpleFuture(fs.map(_.get))
  }

  // EXERCISE 7 (hard): Let's write this function, typically called sequence. No additional primitives are required.
  def sequence[A](l: List[Par[A]]): Par[List[A]] = es => {
    es.submit(new Callable[List[A]] {
      def call() = l.map(x => x(es).get)
    })
  }

  // My own improved version based on the implementation of previous sequence functions
  def sequence_better[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](Par.unit(List()))((x,y) => Par.map2(x,y)(_ :: _))
}