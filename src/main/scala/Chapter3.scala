
package object Chapter3 {

  // this implementation of List was provided in the book:
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // Exercise 2 - definition of tail
    def tail[A](l:List[A]) = l match {
      case Cons(_, tail) => tail
      case _ => Nil
    }

    // Exercise 3 - define drop, that drops the first 'n' elements of a list
    def drop[A](n:Int, l:List[A]):List[A] = l match {
      case _ if n == 0 => l
      case Cons(_, tail) => drop(n-1, tail)
      case _ => Nil
    }

    val l1 = List(1,2,3,4)
    println(drop(3, l1))    // prints Cons(4,Nil)

    // Exercise 4 - implement dropWhile
    type Pred[A] = A => Boolean
    def dropWhile[A](f:Pred[A], l:List[A]):List[A] = l match {
      //case _ if n == 0 => l
      case Cons(x, tail) if f(x) => dropWhile(f, tail)
      case Cons(x, tail) => Cons(x, tail)
      case _ => Nil
    }

    val l2 = List(2,4,6,8,10,11)
    val fEven:Pred[Int] = x => x % 2 == 0
    println(dropWhile(f, l))    // prints Cons(11,Nil)

    val example = Cons(1, Cons(2, Cons(3, Nil)))
    val example2 = List(1,2,3)
    val total = sum(example)

    // Exercise 5 - implement setHead to replace the current element at the head of the list
    def setHead[A](l:List[A], x:A) = l match {
      case Cons(_, tail) => Cons(x, tail)
      case _ => Cons(x, Nil)
    }

    println(setHead(l2, 4))  // returns (4,4,6,8,10,11)

    // Exercise 6 - init: return all elements except the last one
    // TODO: the resulting list is in reverse order - would there be any easy way to fix that?
    def init[A](l:List[A]) = {
      def initR(newL:List[A], oldL:List[A]):List[A] = oldL match {
        case Cons(_, Nil) => newL
        case Cons(head, tail) => initR(Cons(head, newL), tail)
        case _ => Nil
      }
      initR(Nil, l)
    }

    def foldRight[A,B](list: List[A], z: B)(f: (A, B) => B): B = list match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    // Exercises 7, 8 and 9 - to be done on a piece of paper

    // Exercise 10 - length of a list using foldright
    def length[A](l:List[A]): Int = foldRight(l, 0)((x,y) => y + 1)

    // Exercise 11 - implement foldLeft
    def foldLeft[A,B](list: List[A], z: B)(f: (A, B) => B): B = {
      def innerFold(list:List[A], acc: B): B = {
        list match {
          case Nil => acc
          case Cons(head, tail) => innerFold(tail, f(head, acc))
        }
      }

      innerFold(list, z)
    }

    // Exercise 12 - length of a string using foldLeft
    def length_foldLeft[A](l:List[A]): Int = foldLeft(l, 0)((x,y) => y + 1)

    // Exercise 13 - reverse a list using a fold
    // Note: I'm being forced to curry foldLeft because the compiler insists that if
    // I use Nil as the initial fold state, then one of the parameters in the
    // function must be of Nil.type type; using currying seems to help there
    def reverse[A](l:List[A]) = (foldLeft(l, _:List[A])((x,y) => Cons(x, y)))(Nil)

    // Exercise 14-1 (hard) - Can you write foldLeft in terms of foldRight?
    //def foldLeft_withRight[A,B](list: List[A], z: B)(f: (A, B) => B): B = {
      // TODO
    //}

    // Exercise 14-2 (hard) - How about the other way around?
    //def foldRight_withLeft[A,B](list: List[A], z: B)(f: (A, B) => B): B = {
      // TODO
    //}

    // Exercise 15 - implement append in terms of foldLeft or foldRight
    def append[A](l1:List[A], l2:List[A]):List[A] =

    // Exercise 16 (hard) - Write a function that concatenates a list of lists into
    // a single list. Its runtime should be linear in the total length of all lists.
    // Try to use functions we have already defined.
    //
    // Note: seems to work fine but the list is presented in reverse order
    def flatten[A](l:List[List[A]]): List[A] = {
      (foldLeft(l, _:List[A])((x,y) => foldLeft(x, y)((l1,l2) => Cons(l1,l2))))(Nil)
    }

    // Exercise 17 - Write a function that takes a list of integers and modifies each element by adding 1 to it.
    def add1(l:List[Int]) = (foldLeft(l, _:List[Int])((x,y) => Cons(x+1, y)))(Nil)

    // EXERCISE 18: Write a function that turns each value in a List[Double] into a String.
    def doubleToString(l:List[Double]) = (foldLeft(l, _:List[String])((x,y) => Cons(x.toString, y)))(Nil)

    // EXERCISE 19: Write a function map, that generalizes modifying each element in a list while maintaining the structure of the lis
    def map[A,B](list: List[A])(f: A => B): List[B] = {
      (foldLeft(list, _:List[B])((x,y) => Cons(f(x), y)))(Nil)
    }
    // now we can reimplemented add1 and doubleToString
    map(List(1,2,3))(_ + 1)
    map(List(1.0, 2.0, 3.0))(_.toString)
  }
}
