import scala.collection.mutable

object Dijkstra extends App {

  class Graph {

    val nodes : mutable.Set[Node] = mutable.Set()

    def addNode(node: Node) = {
      nodes += node
    }

    def addEdge(from: Node, to: Node, weight: Float) = {
      from.edges += Edge(from, to).withWeight(weight)
      to.edges += Edge(to, from).withWeight(weight)
    }

    def getAllNodes() = {
      nodes
    }

  }



  case class Node(label: String) extends Ordered[Node]{
    val edges: mutable.Set[Edge] = mutable.Set()
    var weight: Float = Float.PositiveInfinity

    def compare(y: Node): Int = this.weight.compare(y.weight)
  }

  case class Edge(node1: Node, node2: Node){
    var weight: Float = Float.PositiveInfinity

    def from: Node = this.node1
    def to: Node = this.node2

    def withWeight(weight: Float): Edge = {
      this.weight = weight
      this
    }
  }




  def compute(start: Node, graph: Graph) : Unit = {
    val unvisited = new mutable.PriorityQueue[Node]()
    start.weight = 0
    unvisited ++= graph.getAllNodes()

    while(unvisited.nonEmpty){
      val currentNode = unvisited.dequeue()
      for (edge: Edge <- currentNode.edges) {
        if (isCloserBy(edge)){
          edge.to.weight = currentNode.weight + edge.weight
          unvisited += edge.to
        }
      }
    }
  }

  def printShortestPaths(graph: Graph) = {
    for (node <- graph.getAllNodes()){
      println(node.label + " -> " +node.weight)
    }
  }

  def isCloserBy(edge: Edge) = {
    edge.from.weight + edge.weight < edge.to.weight
  }


  val node1 = Node("node1")
  val node2 = Node("node2")
  val node3 = Node("node3")
  val node4 = Node("node4")

  val graph = new Graph()
  graph.addNode(node1)
  graph.addNode(node2)
  graph.addNode(node3)
  graph.addNode(node4)

  graph.addEdge(node1, node2, 5)
  graph.addEdge(node1, node4, 3)
  graph.addEdge(node2, node3, 1)
  graph.addEdge(node3, node4, 6)

  compute(node1, graph)
  printShortestPaths(graph)
}
