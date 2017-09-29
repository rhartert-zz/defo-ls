package srte.util

abstract class SegmentDB {
  
  def edges(src: Int, dest: Int): Array[Int]
  
  def nEdges(src: Int, dest: Int): Int
  
  def flow(src: Int, dest: Int, edge: Int, flow: Double): Double
}

object SegmentDB { 
  def apply(topology: Topology, weights: Array[Int], compact: Boolean = false): SegmentDB = {
    val dagsEdges = Array.tabulate(topology.nNodes)(v => {
      Dijkstra.shortestPathTo(v, topology, weights)._1.map(_.toArray)
    })
    val dagsNodes = dagsEdges.map(_.map(_.map(e => topology.edgeDest(e))))
    if (compact) new CompactSegmentDB(topology.nEdges, topology.nNodes, dagsEdges, dagsNodes)
    else new FastSegmentDB(topology.nEdges, topology.nNodes, dagsEdges, dagsNodes)
  }
}