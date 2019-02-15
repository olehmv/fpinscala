//def failingFn(i: Int): Int = {
//  println(0)
//  //  def y: Int = throw new Exception("fail!")
//  val y: Int = throw new Exception("fail!")
//  println(0)
//  try {
//    val x = 42 + 5
//    println(x)
//    x + y
//  }
//  catch {
//    case e: Exception => 43
//  }
//}
//failingFn(12)


def failingFn2(i: Int): Int = {
  try {
    val x = 42 + 5
    //A thrown Exception can be given any type; here we’re annotating it with the type Int.
    x + ((throw new Exception("fail!")): Int)
  }
  catch {
    case e: Exception => 43
  }
}
failingFn2(12)

def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

val absO: Option[Double] => Option[Double] = lift(math.abs)

absO(Option(-2)).get

//EXERCISE 4.3
def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
  case (None, _) => None
  case (_, None) => None
  case (Some(v1), Some(v2)) => Some(f(v1, v2))
}

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch {
    case e: Exception => None
  }

/**
  * Top secret formula for computing an annual car
  * insurance premium from two key factors.
  */
def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
  age * numberOfSpeedingTickets / math.random()
}

def parseInsuranceRateQuote(
                             age: String,
                             numberOfSpeedingTickets: String): Option[Double] = {
  val optAge: Option[Int] = Try {
    age.toInt
  }
  val optTickets: Option[Int] = Try {
    numberOfSpeedingTickets.toInt
  }
  map2(optAge, optTickets)(insuranceRateQuote)
}

parseInsuranceRateQuote("2", "we")

//  EXERCISE 4.2
def variance(xs: Seq[Double]): Option[Double] = {
  def mean(xs: Seq[Double]): Option[Double] = {
    Some(xs.sum / xs.length)
  }

  mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
}
variance(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

//EXERCISE 4.4
def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  a match {
    case Nil => Some(Nil)
    case head :: tail => head.flatMap(h => sequence(tail).map(list => h :: list))
  }
}

sequence(List(Some(1), Some(2), Some(3)))

sequence(List(Some(1), Some(2), None))

//EXERCISE 4.5
def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
  a match {
    case Nil => Some(Nil)
    case head :: tail => {
      traverse(tail)(f) flatMap (list => f(head).map(e => e :: list))
    }
  }
}
traverse(List("e", "2", "3"))(i => Try(i.toInt))

def mapForComp[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C):
Option[C] =
  for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

//EXERCISE 4.7
def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
  es match {
    case Nil=>Right(Nil)
    case head::tail=>head.flatMap(h => sequence(tail).map(list => h :: list))
  }
}
sequence(List(Right(1),Left(new Exception("ex here")),Right(3)))

sequence(List(Right(1),Right(2),Right(3)))



