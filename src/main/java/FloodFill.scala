import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object FloodFill extends App {


  val example = "++++++++\n++%%%%++\n++%%%%++\n++++++++".split("\n").map(str => str.toCharArray.toList.to[ListBuffer]).to[ListBuffer]
  val example2 = "++++++++\n++%%%%++\n++%%%%++\n++++++++".split("\n").map(str => str.toCharArray.toList.to[ListBuffer]).to[ListBuffer]

  def floodFill(originalMotif: Char, newMotif: Char, array: ListBuffer[ListBuffer[Char]], x: Int, y: Int): Unit = {
    val current = array(x)(y)
    if (current != originalMotif)
      ()
    if (current == originalMotif) {
      array(x)(y) = newMotif
      floodFill(originalMotif, newMotif, array, x, y-1)
      floodFill(originalMotif, newMotif, array, x, y+1)
      floodFill(originalMotif, newMotif, array, x+1, y)
      floodFill(originalMotif, newMotif, array, x-1, y)
    }
  }

  /*println(example)
  floodFill('%', '*', example, 1, 3)
  println(example)*/

  def floodFillImproved(originalMotif: Char, newMotif: Char, array: ListBuffer[ListBuffer[Char]], stack: mutable.Queue[Point]): Unit = {
    for (elem <- stack) {
      val x = elem.x
      val y = elem.y
      val current = array(x)(y)
      if (current != originalMotif)
        ()
      if (current == originalMotif) {
        array(x)(y) = newMotif
        stack.enqueue(Point(x, y - 1), Point(x, y + 1), Point(x + 1, y), Point(x - 1, y))
        floodFillImproved(originalMotif, newMotif, array, stack)
      }
    }
  }

  /*val myqueue = new mutable.Queue[Point]()
  myqueue.enqueue(Point(1,3))
  println(example2)
  floodFillImproved('%', '*', example2, myqueue)
  println(example2)*/

  case class Point(x: Int, y: Int)

  def multiThreadedImprovedFloodFill(nbThreads: Int, originalMotif: Char, newMotif: Char,
                                     array: ListBuffer[ListBuffer[Char]]): Unit = {
    // cut in N shards the matrix
    // 1 thread per shard
    // Every thread manipulates its queue, if a point goes beyond the thread' shard, then it enqueues the point in the other thread queue
    val queues = new Array[mutable.Queue[Point]](nbThreads)
    for (e <- queues.indices){
      queues(e) = new mutable.Queue[Point]()
    }
    queues(1).enqueue(Point(1,3))
    new Thread() {
      override def run() = {
        boxedFloodFill(1, nbThreads, originalMotif, newMotif, array, queues)
      }
    }.start()
  }

  def boxedFloodFill(id: Int, nbThreads: Int, originalMotif: Char, newMotif: Char, array: ListBuffer[ListBuffer[Char]],
                     stacks: Array[mutable.Queue[Point]]) : Unit = {
    val stack = stacks(id)
    for (elem <- stack) {
      val x = elem.x
      val y = elem.y
      val current = array(x)(y)
      if (current != originalMotif)
        ()
      if (current == originalMotif) {
        array(x)(y) = newMotif
        boxedEnqueue(id, nbThreads, originalMotif, newMotif, Point(x,y), array, stacks)
        boxedFloodFill(id, nbThreads, originalMotif, newMotif, array, stacks)
      }
    }
  }

  def boxedEnqueue(id: Int, nbThreads: Int, originalMotif: Char, newMotif: Char, current: Point, array: ListBuffer[ListBuffer[Char]], stacks: Array[mutable.Queue[Point]]): Unit = {
    val x = current.x
    val y = current.y
    val nbLinesPerThread = array.size / nbThreads
    val topLimit = id * nbLinesPerThread
    val bottomLimit = topLimit + nbLinesPerThread

    if (y-1 < topLimit){
      stacks(id-1).enqueue(Point(x, y - 1))
      new Thread() {
        override def run() = {
          boxedFloodFill(id-1, nbThreads, originalMotif, newMotif, array, stacks)
        }
      }.start()
    } else {
      stacks(id).enqueue(Point(x, y - 1))
    }

    if (y+1 < bottomLimit){
      stacks(id+1).enqueue(Point(x, y + 1))
      new Thread() {
        override def run() = {
          boxedFloodFill(id+1, nbThreads, originalMotif, newMotif, array, stacks)
        }
      }.start()
    } else {
      stacks(id).enqueue(Point(x, y + 1))
    }
    stacks(id).enqueue(Point(x + 1, y), Point(x - 1, y))
  }

  println(example)
  multiThreadedImprovedFloodFill(4, '%', '*', example)
  Thread.sleep(3000)
  println(example)
}
