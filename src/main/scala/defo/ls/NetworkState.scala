package defo.ls

import defo.core.Topology
import scala.collection.mutable.ArrayBuffer

class NetworkState(topology: Topology, capacities: Array[Double]) {

  private[this] val nEdges = topology.nEdges
  private[this] val nNodes = topology.nNodes

  private[this] val bestFlows = new Array[Double](nEdges)
  private[this] var bestUsage = 0.0

  // Saved state
  private[this] val changed = new Array[Boolean](nEdges)
  private[this] val storedFlows = new Array[Double](nEdges)
  private[this] val storedEdges = new Array[Int](nEdges)
  private[this] var nStoredEdges = 0

  def updateMax(): Unit = {
    bestUsage = 0.0
    var e = nEdges
    while (e > 0) {
      e -= 1
      val usage = bestFlows(e) / capacities(e)
      if (usage > bestUsage) {
        bestUsage = usage
      }
    }
  }

  // Save the current changes so that the current solution becomes the default
  // solution.
  def saveChanges(): Unit = {
    while (nStoredEdges > 0) {
      nStoredEdges -= 1
      val edge = storedEdges(nStoredEdges)
      changed(edge) = false
    }
  }

  // Undo the last changes so that the default solution is restored.
  def undoChanges(): Unit = {
    while (nStoredEdges > 0) {
      nStoredEdges -= 1
      val edge = storedEdges(nStoredEdges)
      changed(edge) = false
      bestFlows(edge) = storedFlows(edge)
    }
  }

  def addFlow(edgeId: Int, flow: Double): Unit = {
    if (!changed(edgeId)) {
      changed(edgeId) = true
      storedFlows(edgeId) = bestFlows(edgeId)
      storedEdges(nStoredEdges) = edgeId
      nStoredEdges += 1
    }
    bestFlows(edgeId) += flow
  }
  
  def changes = storedEdges
  def nChanges = nStoredEdges
  
  def flow(edgeId: Int): Double = bestFlows(edgeId)

  def removeFlow(edgeId: Int, flow: Double): Unit = {
    addFlow(edgeId, -flow)
  }
}
