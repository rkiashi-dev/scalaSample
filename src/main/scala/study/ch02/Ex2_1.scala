package study.ch02

// https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/gettingstarted/GettingStarted.scala

object Ex2_1 {

  def abs(n: Int): Int =
    if( n < 0 ) -n
    else n

  def fractional(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc:Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  // ex 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc1: Int, acc2: Int): Int =
      if ( n < 1 ) 0
      else if ( n == 1 ) acc1
      else go(n-1, acc1+acc2,acc1)

    go(n-1,1,0)
  }

  private def result( x: Int, fmt: String, fnc: Int => Int ) = {
    println(fmt.format( x, fnc(x) ))
  }

  def main(args: Array[String]): Unit = {
    result( -1, "The absolute value of %d is %d", abs )
    result( 1, "The absolute value of %d is %d", abs )
    result( -42, "The absolute value of %d is %d", abs )
    result( 3, "The fractional value of %d is %d", fractional )
    result( 1, "The fib value of %d is %d", fib )
    result( 2, "The fib value of %d is %d", fib )
    result( 3, "The fib value of %d is %d", fib )
    result( 4, "The fib value of %d is %d", fib )
    result( 5, "The fib value of %d is %d", fib )
    result( 6, "The fib value of %d is %d", fib )
  }
}
