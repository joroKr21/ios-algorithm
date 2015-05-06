import scala.util.Random

val r: Random = new Random(12345)
val customers = Array.fill[Double](20,2) { r.nextDouble * 200 }

customers.map(x => x.mkString(" "))


val test = (10 until 20).toArray
test.zipWithIndex
