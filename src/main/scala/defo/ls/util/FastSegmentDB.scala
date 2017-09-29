package srte.util

class FastSegmentDB(
  nEdges: Int,
  nNodes: Int,
  dagsEdges: Array[Array[Array[Int]]],
  dagsNodes: Array[Array[Array[Int]]]
) extends CompactSegmentDB(
  nEdges,
  nNodes,
  dagsEdges,
  dagsNodes
) {
  
  private[this] val segments = Array.ofDim[Array[Int]](nNodes, nNodes)
  private[this] val flows = Array.ofDim[Array[Double]](nNodes, nNodes)
  
  for (s <- 0 until nNodes; t <- 0 until nNodes) {
    if (s != t) {
      buildSegment(s, t, 1.0)
      segments(s)(t) = segmentEdges.take(segmentSize)
      flows(s)(t) = new Array[Double](nEdges)
      var i = segments(s)(t).length
      while (i > 0) {
        i -= 1
        val e = segments(s)(t)(i)
        flows(s)(t)(e) = edgeFlows(e)
      }
    }
  }
            
  override def edges(src: Int, dest: Int): Array[Int] = segments(src)(dest)
  
  override def nEdges(src: Int, dest: Int): Int = segments(src)(dest).length
  
  override def flow(src: Int, dest: Int, edge: Int, bw: Double): Double = flows(src)(dest)(edge) * bw
}