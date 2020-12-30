package study.ch02

object Ex2_2 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    Range.apply(0, as.length-1)
      .filter( index => {
        ordered(as(index), as(index+1))
      })
      .size == as.length-1
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1,2,3,4,6,5)
    val result = isSorted(nums, (X: Int,Y: Int) => { X < Y })

    nums.foreach(println)
    println(result)
  }
}
