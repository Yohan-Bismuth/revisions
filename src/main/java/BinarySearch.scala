object BinarySearch extends App {

  val tab = Array(0, 5, 7, 9, 13, 18, 56, 102, 205)

  def binarySearch(e: Int, array: Array[Int], inf: Int, sup: Int): Boolean = {
    val mid = (inf + sup) / 2
    val elem = array(mid)
    if (sup >= inf) {
      if (e == elem) {
        true
      } else {
        if (e < elem) {
          binarySearch(e, array, inf, mid - 1)
        } else {
          binarySearch(e, array, mid + 1, sup)
        }
      }
    } else false
  }

  println(binarySearch(1, tab, 0, tab.length))
  println(binarySearch(9, tab, 0, tab.length))
  println(binarySearch(13, tab, 2, tab.length - 2))
  println(binarySearch(15, tab, 2, tab.length - 2))
}
