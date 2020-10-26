import scala.annotation.tailrec

object ex2 {

  def minmax(xs: List[Int]) = {
    @tailrec
    def _minmax(_xs: List[Int], min: Int, max: Int): Tuple2[Int, Int] = {
      if(_xs.nonEmpty) {
        _minmax(_xs.tail,
          if(_xs.head < min) _xs.head else min,
          if(_xs.head > max) _xs.head else max)
      } else (min, max)
    }

    _minmax(xs, xs.head, xs.head)
  }

  def main(args: Array[String]): Unit = {
    val (x,y) = minmax(List(4, 6, 1, 9, 6, 1, 8, 6, 4, 2, 9 ,1 ,3 ,5, 10, 0,6, 7))
    println(x,y)
  }
}
