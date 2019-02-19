package laziness

import scala.collection.immutable.Stream.cons
import scala.runtime.Nothing$


//This type looks identical to our List type, except that the Cons data constructor takes
//explicit thunks (() => A and () => Stream[A]) instead of regular strict values. If we wish
//to examine or traverse the Stream, we need to force these thunks

sealed trait Stream[+A] {

  //EXERCISE 5.1
  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(head, tail) => head() :: tail().toList
    }
  }

  //EXERCISE 5.2
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(head, tail) if (n > 0) => Stream.cons(head(), tail() take (n - 1))
      case Cons(head, _) if (n == 1) => Stream.cons(head(), Empty)
      case _ => Empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, tail) if (n > 0) => tail().drop(n - 1)
      case _ => this
    }
  }

  //EXERCISE 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Empty

    }
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  //The arrow => in front of the argument type B means
  //that the function f takes its second argument by name and may choose not to evaluate it
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }


  //Here b is the unevaluated recursive step that folds the tail of the stream. If p(a)
  //returns true, b will never be evaluated and the computation terminates early
  // Since foldRight can terminate the traversal early, we can reuse it to implement
  //exists. We can’t do that with a strict version of foldRight.
  // We’d have to write a specialized recursive exists function to handle early termination.
  // Laziness makes our code more reusable.
  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((h, t) => p(h) || t)

  //EXERCISE 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  //EXERCISE 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())(
      (h, t) =>
        if (p(h))
          Stream.cons(h, t.takeWhile2(p))
        else
          Empty
    )

  //EXERCISE 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, t) => Option[A](h))

  //EXERCISE 5.7
  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream[A]())(
      (h, t) =>
        if (f(h)) {
          Stream.cons(h, t.filter(f))
        } else {
          t.filter(f)
        }
    )
  }

  def map[B](f: A => B): Stream[B] = {
    this match {
      case Cons(h, t) => Stream.cons(f(h()), t().map(f))
      case _ => Empty
    }
  }

  def append[U >: A](x: U): Stream[U] = {
    foldRight(Stream.cons(x, Empty))((h, t) => Stream.cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream[B]())((h, t) => f(h).concat(t))
  }

  def concat[U >: A](xs: Stream[U]): Stream[U] = {
    foldRight(xs)((h, t) => t.append(h))
  }

  //EXERCISE 5.8
  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  //EXERCISE 5.9
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  //EXERCISE 5.10
  // 0, 1, 1, 2, 3, 5, 8
  def fibs(): Stream[Int] = {
    def go(f: Int, s: Int): Stream[Int] = {
      Stream.cons(f, go(s, f + s))
    }

    go(0, 1)
  }

  //EXERCISE 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, z)) => Stream.cons(a, unfold(z)(f))
      case None => Stream.empty
    }
  }

  def fibsViaUnfold(): Stream[Int] = {
    unfold((0, 1)) {
      case (f0, f1) => Some(f0, (f1, f0 + f1))
    }
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n) {
      case n => Some(n, n + 1)
    }
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a) {
      case a => Some(a, a)
    }
  }

  def ones: Stream[Int] = {
    unfold(1) {
      case i => Some(i, i)
    }
  }

  //EXERCISE 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), next) if (next > 0) => Some((h(), (t(), next - 1)))
      case (Cons(h, t), next) if (next == 1) => Some((h(), (Empty, next - 1)))
      case _ => None
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold((this, true)) {
      case (Cons(h, t), condition) if (p(h())) => Some((h(), (t(), condition)))
      case (Cons(h, t), false) => Some(h(), (Empty, false))
      case _ => None
    }
  }

  def zipWith[B, C](ys: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, ys)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    def zipHelper[C](ys: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = {
      unfold((this, ys)) {
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Empty))
        case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Empty -> t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
      }
    }
    zipHelper(s2)((a, b) => (a, b))
  }
  //EXERCISE 5.14

  //EXERCISE 5.15


}

case object Empty extends Stream[Nothing]

//A nonempty stream consists of a head and a tail,
//which are both non-strict. Due to technical
//limitations, these are thunks that must be
//explicitly forced, rather than by-name parameters.
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

//Scala uses subtyping to represent data constructors, but we almost always want to infer Stream as
//the type, not Cons or Empty. Making smart constructors that return the base type is a common trick
object Stream {

  def main(args: Array[String]): Unit = {
    val stream = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    println(stream.toList)
    println(stream.take(5).toList)
    println(stream.drop(3).toList)
    println(stream.takeWhile(h => h > 0 && h <= 5).toList)
    println(stream.forAll(h => h == h))
    println(stream.forAll(h => h == 1))
    println(stream.takeWhile2(h => h < 3).toList)
    println(Empty.headOption)
    println(stream.headOption)
    println(stream.filter(h => h % 2 == 0).toList)
    println(stream.map(h => h.toDouble).toList)
    println(stream.append(111).toList)
    println(stream.foldRight(0)(_ + _))
    println(stream.concat(Stream(11, 12, 13, 14, 15)).toList)
    println(Stream(1, Stream(2, 3), 4).flatMap(e => Stream(e)).toList)
    println(stream.constant(1).map(_ + 5).take(5).toList)
    println(stream.from(6).take(5).toList)
    println(stream.fibs().take(7).toList)
    println(stream.unfold(5)(i => Option(i, i + i)).take(5).toList)
    println(stream.fibsViaUnfold().take(7).toList)
    println(stream.fromViaUnfold(6).take(5).toList)
    println(stream.constantViaUnfold(1).map(_ + 5).take(5).toList)
    println(stream.ones.take(5).toList)
    println(stream.mapViaUnfold(i => i.toDouble).toList)
    println(stream.takeViaUnfold(5).toList)
    println(stream.takeWhileViaUnfold(h => h > 0 && h <= 5).toList)
    println(stream.takeWhileViaUnfold(h => h < 5).toList)
    println(stream.zipWith(stream)((a, b) => (a, b)).toList)
    println(stream.zipAll(stream).toList)
  }

  //A smart constructor for creating a nonempty stream.
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {


    //Cache the head and tail as lazy values to avoid repeated evaluation
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  //A smart constructor for creating an empty stream of a particular type.
  def empty[A]: Stream[A] = Empty

  //A convenient variable-argument method for constructing a Stream from multiple elements.
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}