
package object List {

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
    println(dropWhile(fEven, l2))    // prints Cons(11,Nil)

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
    def append[A](l1:List[A], l2:List[A]):List[A] = {
      foldLeft(l1, l2)((x,y) => Cons(x,y))
    }

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

    // Exercise 20: implement the filter function that will remove all elements from the list unless they match the given predicate
    def filter[A](l: List[A])(f: Pred[A]) = {
      (foldLeft(l, _:List[A])((x,y) => if(f(x)) Cons(x, y); else y))(Nil)
    }

    // Exercise 21: implement flatMap, that works like map except that the
    // function given will return a list instead of a single result, and that
    // list should be inserted into the final resulting list
    def flatMap[A,B](l:List[A])(f: A => List[B]): List[B] = {
      foldLeft(l, List[B]())((x,y) => append(f(x), y))
    }
    // Note: it logically works, but the order of the elements is kind of funny

    // Exercise 22: can you use flatMap to implement filter?
    def filter_flatMap[A](l: List[A])(f: Pred[A]): List[A] = {
      flatMap(l)(x => if(f(x)) List(x); else List[A]())
    }

    // Exercise 23: write a function that accepts two lists and constructs a new
    // list by adding corresponding elements. For example List(1,2,3) and 
    // List(4,5,6) becomes List(5,7,9)
    def addIntLists(l1:List[Int], l2:List[Int]):List[Int] = {
      def innerAdd(lTotal:List[Int], l1:List[Int], l2:List[Int]): List[Int] = {
        (l1,l2) match {
          case (Nil, _) => lTotal
          case (Cons(h1, t1), Cons(h2, t2)) => innerAdd(Cons(h1 + h2, lTotal), t1, t2)
        }
      }
      innerAdd(List[Int](), l1, l2)
      
    }
    val lInt1 = List(1,2,3)
    val lInt2 = List(4,5,6)
    val lIntSum = addIntLists(lInt1, lInt2)

    // Exercise 24: generalize the function you just wrote so that it's not
    // specific to integers or addition
    def addLists[A,B,C](l1:List[A], l2:List[B])(f: (A,B) => C):List[C] = {
      def innerAdd(lTotal:List[C], l1:List[A], l2:List[B]): List[C] = {
        (l1,l2) match {
          case (_, Nil) => lTotal
          case (Nil, _) => lTotal
          case (Cons(h1, t1), Cons(h2, t2)) => innerAdd(Cons(f(h1,h2), lTotal), t1, t2)
        }
      }
      innerAdd(List[C](), l1, l2)
    }
    // and now we can redefine addIntLists
    def betterAddIntLists(l1:List[Int], l2:List[Int]) = addLists(lInt1, lInt2)(_ + _)
    val lIntSum2 = betterAddIntLists(lInt1, lInt2)

    // Exercise 25 (hard): implement hasSubsequence for checking whether a list
    // contains another List as a subsequence. For instance, List(1,2,3,4) would
    // have List(1,2), List(2,3) and List(4) as subsequences
    
    // helper method that returns true if the given list has the second
    // list as a prefix, e.g. List(1,2,3) has prefix List(1), List(1,2) and 
    // List(1,2,3) but not List(2,3)
    def hasPrefix[A](list:List[A], prefix:List[A]): Boolean = {
      def innerHasPrefix(acc: Boolean, l1:List[A], l2:List[A]): Boolean = {
        (acc, l1,l2) match {
          case (false, _, _) => false
          case (true, _, Nil) => true
          case (true, Nil, _) => false
          case (true, Cons(h1, t1), Cons(h2, t2)) => innerHasPrefix(acc && (h1 == h2), t1, t2)
        }
      }
      
      innerHasPrefix(true, list, prefix)      
    }

    // Seems to work, though I don't think it's the most optiomal implementation ever
    def hasSubsequence[A](list: List[A], sub: List[A]): Boolean = {
      def innerHasSubsequence[A](acc: Boolean, l1:List[A], sub:List[A]): Boolean = {
        // if the accumulator is true, we're good
        // if the lists are not empty yet, deconstruct the first and check
        // if the second is a prefix of the first minus its head
        // if either of the lists is over, return whatever value the boolean accumulator has
        (acc, l1, sub) match {
          case(true, _, _) => true  
          case(_, Cons(h1, t1), l) => innerHasSubsequence(hasPrefix(Cons(h1, t1), l), t1, l)
          case(acc, Nil, l) => acc
          case(acc, l, Nil) => acc
        }
      }

      innerHasSubsequence(false, list, sub)
    }
  }
}

package object Tree {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  val t1 = Branch(Leaf(1), Leaf(2))
  val t2 = Branch(Branch(t1, t1), Branch(t1, t1))
  val t3 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(10), Leaf(11)))
  val t4 = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4))

  // Exercise 26 - write a function size that counts the number of nodes
  // in a tree
  def size[A](t:Tree[A]): Int = {
    // note: this is not tail recursive
    t match {
      case Branch(left, right) => size(left) + size(right) + 1
      case _ => 1
    }
  }

  // Exercise 27 - a function that finds the highest value in a Tree[Int]
  def max(t:Tree[Int]): Int = {
    // again, not case recursive - but it could be
    def innerMax(maxVal: Int, t:Tree[Int]):Int = t match {
      case Branch(left, right) => innerMax(maxVal, right) max innerMax(maxVal, left) 
      case Leaf(v) if(v > maxVal) => v
      case Leaf(v) => maxVal
    }

    innerMax(0, t)
  }

  // Exercise 28 - function depth that find the maximum path length
  // from the roof of a tree to any leaf
  def depth[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => 1 + (depth(left) max depth(right))
    case _ => 1
  }

  // Exercise 29 - map function
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(v) => Leaf(f(v))
  }

  // Exercise 30 - Generalize size, maximum, depth, and map, writing a new 
  // function fold that abstracts over their similarities. Reimplement them in terms of
  // this more general function. Can you draw an analogy between this fold function 
  // and the left and right folds for List?
  def fold_Bad[A,B](t:Tree[A], z:B)(f:(A,B) => B): B = t match {
    case Branch(left, right) => fold_Bad(left, fold_Bad(right, z)(f))(f)
    case Leaf(v) => f(v, z)
  }
  // Please note that the signature above was wrong; from the solutions, the following was the
  // correct signature:
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)    
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size_fold[A](t:Tree[A]): Int = {
    fold(t)(a=>1)(1 + _ + _)
  }

  def max_fold(t:Tree[Int]): Int = {
    fold(t)(a=>a)(_ max _) 
  }

  // NOTE: I did not get these two right, so these are the implementations from the solution
  def depth_fold[A](t:Tree[A]): Int = {
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))
  }

  def map_fold[A,B](t:Tree[A])(f:A => B): Tree[B] = {
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
  }
}