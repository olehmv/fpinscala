object Study {

  def main(args: Array[String]): Unit = {

    println(formatResult("fib number", 10, fib))
    println(isSorted(Array.range(1, 10), (a: Int, b: Int) => a > b))

  }

  /** Monomorphic function */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, first: Int, second: Int): Int = {
      if (n == 0)
        first
      else go(n - 1, second = first + second, first = second)
    }

    go(n, 0, 1)
  }

  /** Higher order function
    * Monomorphic function */
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  /** Polymorphic function */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }
}