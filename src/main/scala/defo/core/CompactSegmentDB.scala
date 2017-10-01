package defo.core

class CompactSegmentDB(
  nEdges: Int,
  nNodes: Int,
  dagsEdges: Array[Array[Array[Int]]],
  dagsNodes: Array[Array[Array[Int]]]) extends SegmentDB {

  // Cache
  protected val segmentEdges = new Array[Int](nEdges)
  protected val segmentFlows = new Array[Double](nEdges)
  protected val edgeFlows = new Array[Double](nEdges)
  protected var segmentSize = 0
  protected var segmentSrc = 0
  protected var segmentDest = 0

  private[this] val nInEdges = new Array[Int](nNodes)
  private[this] val inFlows = new Array[Double](nNodes)
  private[this] val stack = new Array[Int](nEdges)
  private[this] var stackSize = 0

  protected def buildSegment(src: Int, dest: Int, flow: Double) = {
    val dagNodes = dagsNodes(dest)
    val dagEdges = dagsEdges(dest)
    segmentSize = 0
    // Compute the in degree of each node.
    stack(0) = src
    stackSize = 1
    while (stackSize > 0) {
      stackSize -= 1
      val u = stack(stackSize)
      val nodes = dagNodes(u)
      var i = nodes.length
      while (i > 0) {
        i -= 1
        val v = nodes(i)
        nInEdges(v) += 1
        if (nInEdges(v) == 1) {
          stack(stackSize) = v
          stackSize += 1
        }
      }
    }
    // Compute the segment.
    stack(0) = src
    inFlows(src) = flow
    stackSize = 1
    while (stackSize > 0) {
      stackSize -= 1
      val u = stack(stackSize)
      val edges = dagEdges(u)
      val nodes = dagNodes(u)
      val flow = inFlows(u) / edges.length
      inFlows(u) = 0
      var i = edges.length
      while (i > 0) {
        i -= 1
        val e = edges(i)
        segmentFlows(segmentSize) = flow
        segmentEdges(segmentSize) = e
        edgeFlows(e) = flow
        segmentSize += 1
        val v = nodes(i)
        inFlows(v) += flow
        nInEdges(v) -= 1
        if (nInEdges(v) == 0) {
          stack(stackSize) = v
          stackSize += 1
        }
      }
    }
  }

  override def edges(src: Int, dest: Int): Array[Int] = {
    if (segmentSrc != src || segmentDest != dest) buildSegment(src, dest, 1.0)
    segmentEdges
  }
  
  override def nEdges(src: Int, dest: Int): Int = {
    if (segmentSrc != src || segmentDest != dest) buildSegment(src, dest, 1.0)
    segmentSize
  }

  override def flow(src: Int, dest: Int, edge: Int, flow: Double): Double = {
    if (segmentSrc != src || segmentDest != dest) buildSegment(src, dest, flow)
    edgeFlows(edge)
  }
}