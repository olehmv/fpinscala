package errorhandling


//EXERCISE 4.1
sealed trait Option[+A] {

  //  EXERCISE 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] = {
      Some(xs.sum / xs.length)
    }

    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  //EXERCISE 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(v1), Some(v2)) => Some(f(v1, v2))
  }

  //EXERCISE 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case head :: tail => head.flatMap(h => sequence(tail).map(list => h :: list))
    }
  }


  def map3[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C):
  Option[C] =
    a flatMap (aa =>
      b map (bb =>
        f(aa, bb)))

  def map4[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C):
  Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(v) => Some(v)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(v) => {
      if (f(v)) {
        Some(v)
      } else {
        None
      }
    }
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option extends App {

  println(Employee("Joee", "IT", None).lookupByName("Joe").map(_.department))

  println(Employee("Joe", "IT", Some("M OI")).lookupByName("Joe").flatMap(_.manager))

  println(Employee("Joee", "IT", Some("M OI")).lookupByName("Joe").map(_.department).getOrElse("Default Dept."))
}

case class Employee(name: String, department: String, manager: Option[String]) {

  def lookupByName(name: String): Option[Employee] = {
    if (this.name == name) {
      Some(this)
    } else {
      None
    }
  }
}