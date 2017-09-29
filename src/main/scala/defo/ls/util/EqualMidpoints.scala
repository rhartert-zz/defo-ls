package srte.util

import scala.collection.mutable.Map

object EqualMidpoints {
  
  def build(topology: Topology, segments: SegmentDB): Array[Array[Array[Int]]] = {
    
    val nNodes = topology.nNodes
    val midpoints = Array.tabulate(nNodes, nNodes)((src, dest) => {
      if (src == dest) new Array[Int](0)
      else {
        val map = Map[String, Int]()
        // No midpoints.
        map += (segments.edges(src, dest).sorted.mkString("")) -> -1
        
        for (n <- 0 until nNodes if n != src && n != dest) {
          val s1 = segments.edges(src, n)
          val s2 = segments.edges(n, dest)
          val s = (s1 ++ s2).distinct.sorted.mkString("")
          
          if (!map.contains(s)) {
            map += s -> n
          }
        }
        
        map.values.toArray.sorted
      }
    })
    
    return midpoints
  }
  
}