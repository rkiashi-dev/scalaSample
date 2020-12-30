package sample

object F1_Basic {

  def add( x: Int, y: Int): Int =
    x + y

  def addThenMultiply(x:Int,y:Int)(multi:Int): Int =
    (x+y)*multi

  def abs( x: Int ): Int =
    if( x < 0 ) -x
    else x

  def calc( x: Int, fn: Int => Int ): Int =
    x * fn(x)

  def swap[A]( items: Array[A], x: Int, y: Int): Unit = {
    if(x == y) return

    val swap = items(x)
    items(x) = items(y)
    items(y) = swap
  }

  def bubbleSort[A]( items: Array[A], compare: (A,A) => Int): Unit =
  {
    val range = Range(0,items.length-1)
    range.foreach( index => {
      var min = items(index)
      var target = index
      val nextRange = Range(index+1,items.length)
      nextRange.foreach( inner => {
        if( compare(min,items(inner)) > 0 ) {
          min = items(inner)
          target = inner
        }
      })
      swap(items,index,target)
    })
  }

  def main(args: Array[String]): Unit = {
    val values = Array(4,3,2,1)
    println("Not Sort")
    values.foreach( v => println(v))
    bubbleSort[Int](values,(x,y) => x - y)
    println("Sort")
    values.foreach( v => println(v))

    println("HelloWorld!")
    println( add(1,2))
    println( abs(-1))
    println( abs(1))
    println( calc(1, x=> { x - 3 }))
    println( addThenMultiply(1,2)(3))

    new Greeter("a", "b").greet("zz")
  }

}

class Greeter(prefix: String, suffix: String) {
  def greet(name: String): Unit =
    println(prefix + name + suffix)
}
