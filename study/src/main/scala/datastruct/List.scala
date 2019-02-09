package src.main.scala.datastruct

import scala.util.control.TailCalls.TailRec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {


  def main(args: Array[String]): Unit = {

    print(hasSubsequence(List(1, 2, 3), List(2, 3)))

  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def drop1[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) {
      l
    } else {
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
    }
  }

  def dropWhile3[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def dropWhile1[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => sys.error("List is empty")
      case Cons(h, t) => {
        if (f(h)) {
          dropWhile(t, f)
        } else {
          l
        }
      }
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }


  // Placing f in its own argument group after as and z lets type inference determine the input types to f
  def foldRight1[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))


  def foldLeft1[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  }


  //EXERCISE 3.2
  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => sys.error("Empty")
      case Cons(_, t) => t
    }
  }

  //EXERCISE 3.3
  def setHead[A](head: A, list: List[A]): List[A] = {
    list match {
      case Nil => sys.error("Empty")
      case Cons(h, t) => Cons(head, t)
    }
  }

  //EXERCISE 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) {
      l
    } else {
      l match {
        case Nil => sys.error("Empty")
        case Cons(h, t) => drop(t, n - 1)
      }
    }
  }

  //EXERCISE 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => sys.error("Empty")
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }
  }


  //EXERCISE 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("Empty")
      case Cons(h1, Cons(h2, Nil)) => Cons(h1, Nil)
      case Cons(h, t) => append(List(h), init(t))
    }
  }

  // When a function definition contains multiple argument groups,
  // type information flows from left to right across these argument groups.
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile2(t)(f)
      case _ => as
    }

  // Placing f in its own argument group after as and z
  // lets type inference determine the input types to f
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      //List(1,2,3)
      //Cons(1, Cons(2, Cons(3,Nil)))
      //f (1, f (2, f (3, z)))
      //sum2 -> (1 + (2 + (3 + 0))) , product2 -> (1 * (2 * (3 * 1)))
      //
      //    foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x,y) => x + y)
      //    1 + foldRight(Cons(2, Cons(3, Nil)), 0)((x,y) => x + y)
      //    1 + (2 + foldRight(Cons(3, Nil), 0)((x,y) => x + y))
      //    1 + (2 + (3 + (foldRight(Nil, 0)((x,y) => x + y))))
      //    1 + (2 + (3 + (0)))
      //    6
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  //EXERCISE 3.7
  //Can product, implemented using foldRight, immediately halt the recursion and
  //return 0.0 if it encounters a 0.0? NO
  def productT(ns: List[Double]) =
    foldRight(ns, 1.0)((x, y) => if (x == 0) 3 else x * y)

  //EXERCISE 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  //EXERCISE 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f);
    }
  }

  //EXERCISE 3.11
  def sum3(ns: List[Int]) = {
    foldLeft(ns, 0)(_ + _)
  }

  def product3(ns: List[Int]) = {
    foldLeft(ns, 1)(_ * _)
  }

  //EXERCISE 3.12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((z, x) => Cons(x, z))
  }

  //EXERCISE 3.13


  //EXERCISE 3.14
  def append[A](e: A, as: List[A]) = {
    //    foldLeft(as,List[A](e))((z,x)=>Cons(x,z))
    foldRight(as, List[A](e))((x, z) => Cons(x, z))
  }

  //EXERCISE 3.15
  /**
    * Concat two lists into one
    *
    * @param as1
    * @param as2
    * @tparam A
    * @return new List[A]
    */
  @annotation.tailrec
  def concat[A](as1: List[A], as2: List[A]): List[A] = {
    as2 match {
      case Nil => as1
      case Cons(x, xs) => concat(append(x, as1), xs)
    }
  }

  //EXERCISE 3.16
  def addOne(is: List[Int]): List[Int] = {
    //foldRight(is,List[Int]())((x,z)=>Cons(x+1,z))
    is match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, addOne(t))
    }
  }

  //EXERCISE 3.17
  def doubleToString(ds: List[Double]): List[String] = {
    //foldLeft(ds,List[String]())((z,x)=>Cons(x.toString,z)) // return list of string in reverse order
    //foldRight(ds,List[String]())((x,z)=>Cons(x.toString,z))
    ds match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, doubleToString(t))
    }
  }

  //EXERCISE 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }

  //EXERCISE 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) {
        Cons(h, filter(t)(f))
      } else {
        filter(t)(f)
      }
    }
  }

  //EXERCISE 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => concat(f(h), flatMap(t)(f))
    }
  }

  //EXERCISE 3.21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  //EXERCISE 3.22


  //EXERCISE 3.23
  @annotation.tailrec
  def zipWith[A](as1: List[A], as2: List[A], as3: List[A] = List[A]())(f: (A, A) => A): List[A] = {
    as1 match {
      case Nil => as3
      case Cons(h1, t1) => {
        as2 match {
          case Nil => as3
          case Cons(h2, t2) => zipWith(t1, t2, append(f(h1, h2), as3))(f)
        }
      }
    }
  }


}

