package errorhandling

//EXERCISE 4.6
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(v) => Right(f(v))
      case Left(v) => Left(v)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(v) => f(v)
      case Left(v) => Left(v)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(v) => Left(v)
      case Right(v) => b(v)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
  Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }

  //EXERCISE 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
      case Nil=>Right(Nil)
      case head::tail=>head.flatMap(h => sequence(tail).map(list => h :: list))
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

