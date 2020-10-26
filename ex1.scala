import scala.math.{sin, cos, tan, sqrt, log, exp, pow}

object ex1 {

  def calculate(xs: Array[String]): Double = {
    val unaryMap = Map(
      "sin" -> sin _,
      "cos" -> cos _,
      "tan" -> tan _,
      "sqrt" -> sqrt _,
      "log" -> log _,
      "exp" -> exp _)
    val binaryMap = Map(
      "+" -> ((x:Double,y:Double) => x+y),
      "-" -> ((x:Double,y:Double) => x-y),
      "*" -> ((x:Double,y:Double) => x*y),
      "/" -> ((x:Double,y:Double) => x/y),
      "^" -> pow _)

   if(xs.length == 2) {
     val (f, x) = (unaryMap(xs(0)), xs(1).toDouble)
     f(x)
   }
   else if(xs.length == 3) {
     val (f, x, y) = (binaryMap(xs(1)), xs(0).toDouble, xs(2).toDouble)
     f(x,y)
   }
   else 0.0
  }

  def main(args: Array[String]): Unit = {
    println(calculate(Array("2", "+", "3")))
    println(calculate(Array("sin", "0.23")))

  }
}
