import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Random

object QuickSortMultiThreaded extends App {

  val randomArray: Array[Int] = (for (i <- 0 to 10000) yield Random.nextInt(1000)).toArray

  def quickSort(array: Array[Int]): Array[Int] = {
    if (array.length < 2)
      array
    else {
      val pivot = array(array.length / 2)
      Array.concat(
        quickSort(array.filter(elem => elem < pivot)),
        array.filter(elem => elem == pivot),
        quickSort(array.filter(elem => elem > pivot)))
    }
  }

  val nbIter = 0


  val start = System.currentTimeMillis()
  for (_ <- 0 to nbIter) {
    quickSort(randomArray)
  }
  println(System.currentTimeMillis() - start)

  implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global


  def parallelQuicksort(arr: Array[Int])(implicit executionContext:ExecutionContext) : Array[Int]= {
    if (arr.length < 2) {
      arr
    }
    else {
      val pivot = arr(arr.length / 2)
      val p1 = arr.filter(elem => elem < pivot)
      val p2 = arr.filter(elem => elem == pivot)
      val p3 = arr.filter(elem => elem > pivot)

      val f_value: Future[Array[Int]] = for {
        v1 <- Future(parallelQuicksort(p1))
        v3 <- Future(parallelQuicksort(p3))
      } yield Array.concat(v1, p2, v3)

      Await.result(f_value, Duration.Inf)
    }
  }

  val start4 = System.currentTimeMillis()
  for (_ <- 0 to nbIter) {
    parallelQuicksort(randomArray)
  }
  println(System.currentTimeMillis() - start4)

  def parallelQuicksortBeauJeu(arr: Array[Int])(implicit executionContext:ExecutionContext) : Array[Int]= {
    if (arr.length < 2) {
      arr
    }
    else {
      val pivot = arr(arr.length / 2)
      val p1 = arr.filter(elem => elem < pivot)
      val p2 = arr.filter(elem => elem == pivot)
      val p3 = arr.filter(elem => elem > pivot)


      val futures : Future[(Array[Int],Array[Int])] = Future{(parallelQuicksortBeauJeu(p1), parallelQuicksortBeauJeu(p3))}

      val f_value= for {
        currents <- futures
      } yield Array.concat(currents._1, p2, currents._2)

      Await.result(f_value, Duration.Inf)
    }
  }

  val start5 = System.currentTimeMillis()
  for (_ <- 0 to nbIter) {
    parallelQuicksortBeauJeu(randomArray)
  }
  println(System.currentTimeMillis() - start5)

}
