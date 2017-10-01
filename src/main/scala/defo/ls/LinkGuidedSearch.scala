package defo.ls

import scala.util.Random
import defo.core.Topology
import defo.core.SegmentDB
import defo.core.Demand
import defo.util.Results
import defo.util.CumulativeTree
import defo.util.CumulativeTreeEx

class LinkGuidedSearch(topology: Topology, capacities: Array[Double], ecmps: SegmentDB, demands: Array[Demand], verbous: Boolean) {

  private[this] val rand = new Random(0)

  private[this] var alpha = 8.0

  private[this] val maxMidpoints = 2;

  private[this] val nEdges = topology.nEdges
  private[this] val nNodes = topology.nNodes
  private[this] val nDemands = demands.length
  
  private[this] val state = new NetworkState(nEdges)

  private[this] var bestUsage = 0.0

  // The SR Path used to forward the demand in the network. 
  // They are initialized with no midpoint.
  private[this] val srPaths = Array.tabulate(nDemands)(d => SRPath(demands(d), maxMidpoints))

  // Best move
  private final val NONE = 0
  private final val RESET = 1
  private final val INSERT = 2
  private final val REPLACE = 3
  private final val REMOVE = 4

  private[this] var moveType: Int = NONE
  private[this] var movePath = -1
  private[this] var movePosition = -1
  private[this] var moveMidpoint = -1
  private[this] var moveScore = 0.0

  // Cumulative trees for fast selection
  private[this] val edgeTree = new CumulativeTree(nEdges)
  private[this] val demandTree = Array.fill(nEdges)(new CumulativeTreeEx(nNodes))

  var nMoves = 0L
  var nIterations = 0L

  def solve(timeLimit: Long, objective: Double): Results = {
    initialSolution()
    updateMax()

    //if (verbous) println("init usage " + bestUsage)

    val initTime = System.currentTimeMillis
    val t = initTime + timeLimit
    var i = 200

    val maxIterations = 5000000

    while (bestUsage >= objective && System.currentTimeMillis < t && nIterations < maxIterations) {

      nIterations += 1

      if (nIterations % 1000 == 0) {
        alpha = math.max(0, alpha - 1)
      }

      neighborhood()

      if (moveType != NONE) {
        //println("------")
        //println(srPaths(movePath))
      }

      moveType match {
        case RESET => {
          //println("RESET")
          resetMove(movePath, true)
          srPaths(movePath).clear()
        }
        case INSERT => {
          //println("INSERT " + moveMidpoint + " at " + movePosition)
          insertMove(movePath, movePosition, moveMidpoint, true)
          srPaths(movePath).insert(movePosition, moveMidpoint)
        }
        case REPLACE => {
          //println("REPLACE " + moveMidpoint + " at " + movePosition)
          replaceMove(movePath, movePosition, moveMidpoint, true)
          srPaths(movePath).substitute(movePosition, moveMidpoint)
        }
        case REMOVE => {
          //println("REMOVE at " + movePosition)
          removeMove(movePath, movePosition, true)
          srPaths(movePath).remove(movePosition)
        }
        case _ => {}
      }

      if (moveType != NONE) {
        //println(srPaths(movePath))
      }

      // Update best score.
      if (moveType != NONE) {
        evaluateMove(true)
        state.saveChanges()
        updateMax()
      }
    }

    return new Results(System.currentTimeMillis() - initTime, bestUsage, false)
  }

  private def updateMax(): Unit = {
    bestUsage = 0.0
    var e = nEdges
    while (e > 0) {
      e -= 1
      val usage = state.flow(e) / capacities(e)
      if (usage > bestUsage) {
        bestUsage = usage
      }
    }
  }

  private def selectEdge(): Int = {
    edgeTree.get(rand.nextDouble() * edgeTree.totalWeight)
  }

  private def selectMaxEdge(): Int = {
    var maxLoad = Double.MinValue
    var maxEdge = 0
    var e = nEdges
    while (e > 0) {
      e -= 1
      val load = state.flow(e) / capacities(e)
      if (maxLoad < load) {
        maxEdge = e
        maxLoad = load
      }
    }
    return maxEdge
  }

  private def selectDemand(e: Int): Int = {
    demandTree(e).get(rand.nextDouble() * demandTree(e).totalWeight)
  }

  @inline private def evaluateMove(apply: Boolean): Double = {
    var usage = 0.0
    var i = state.nChanges
    val changes = state.changes
    while (i > 0) {
      i -= 1
      val e = changes(i)
      val u = state.flow(e) / capacities(e)
      if (apply) edgeTree.changeWeight(e, math.pow(u, alpha))
      usage = math.max(usage, u)
    }
    usage
  }

  @inline private def removeFlow(d: Int, s: Int, t: Int, bw: Double, apply: Boolean): Unit = {
    val edges = ecmps.edges(s, t)
    var i = ecmps.nEdges(s, t)
    while (i > 0) {
      i -= 1
      val e = edges(i)
      val load = ecmps.flow(s, t, e, bw)
      if (apply) demandTree(e).remove(d)
      state.removeFlow(e, load)
    }
  }

  @inline private def addFlow(d: Int, s: Int, t: Int, bw: Double, apply: Boolean): Unit = {
    val edges = ecmps.edges(s, t)
    var i = ecmps.nEdges(s, t)
    while (i > 0) {
      i -= 1
      val e = edges(i)
      val load = ecmps.flow(s, t, e, bw)
      if (apply) demandTree(e).insert(d, load)
      state.addFlow(e, load)
    }
  }

  private def initialSolution(): Unit = {
    var d = nDemands
    while (d > 0) {
      d -= 1
      val srPath = srPaths(d)
      srPath.clear()
      addFlow(d, srPath.src, srPath.dest, srPath.bw, true)
    }
    var e = nEdges
    while (e > 0) {
      e -= 1
      val usage = state.flow(e) / capacities(e)
      edgeTree.changeWeight(e, math.pow(usage, alpha))
    }
    state.saveChanges()
  }

  private def neighborhood(): Unit = {
    // Reset the previous move.
    moveType = NONE
    moveScore = Double.MaxValue

    // Select the next edge and store its current load to be minimized.
    val e = selectEdge()
    moveScore = state.flow(e)

    val a = demandTree(e).keys.toArray
    val it = a.sortBy(d => -srPaths(d).bw).iterator
    while (it.hasNext) {
      val d = it.next()
      val srPath = srPaths(d)

      // Clear saved state
      state.undoChanges()

      // Reset move 
      if (srPath.canRemove) {
        resetMove(d, false)
        val usage = evaluateMove(false)
        val u = state.flow(e)
        state.undoChanges()
        if (u < moveScore && usage <= bestUsage) {
          moveScore = u
          moveType = RESET
          movePath = d
        }
      }

      // Insert move
      if (srPath.canInsert) {
        var pos = 1
        while (pos < srPath.length) {
          val s = srPath(pos - 1)
          val t = srPath(pos)
          var m = nNodes
          while (m > 0) {
            m -= 1
            // Try to insert m at pos.
            if (m != s && m != t) {
              insertMove(d, pos, m, false)
              val usage = evaluateMove(false)
              val u = state.flow(e)
              state.undoChanges()
              if (u < moveScore && usage <= bestUsage) {
                moveScore = u
                moveType = INSERT
                movePath = d
                movePosition = pos
                moveMidpoint = m
              }
            }
          }
          pos += 1
        }
      }

      // Replace move
      if (srPath.canRemove) {
        var pos = 1
        while (pos < srPath.length - 1) {
          val old = srPath(pos)
          val s = srPath(pos - 1)
          val t = srPath(pos + 1)
          var m = nNodes
          while (m > 0) {
            m -= 1
            // Try to insert m at pos.
            if (m != s && m != t && m != old) {
              replaceMove(d, pos, m, false)
              val usage = evaluateMove(false)
              val u = state.flow(e)
              state.undoChanges()
              if (u < moveScore && usage <= bestUsage) {
                moveScore = u
                moveType = REPLACE
                movePath = d
                movePosition = pos
                moveMidpoint = m
              }
            }
          }
          pos += 1
        }
      }

      // Remove move
      if (srPath.length > 3) {
        var pos = 1
        while (pos < srPath.length - 1) {
          removeMove(d, pos, false)
          val usage = evaluateMove(false)
          val u = state.flow(e)
          state.undoChanges()
          if (u < moveScore && usage <= bestUsage) {
            moveScore = u
            moveType = REMOVE
            movePath = d
            movePosition = pos
          }
          pos += 1
        }
      }

      if (moveType != NONE) return
    }
  }

  private def resetMove(d: Int, apply: Boolean): Unit = {
    nMoves += 1
    val srPath = srPaths(d)
    var i = 1
    while (i < srPath.length) {
      removeFlow(d, srPath(i - 1), srPath(i), srPath.bw, apply)
      i += 1
    }
    addFlow(d, srPath.src, srPath.dest, srPath.bw, apply)
  }

  private def insertMove(d: Int, pos: Int, midpoint: Int, apply: Boolean): Unit = {
    nMoves += 1
    val srPath = srPaths(d)
    val s = srPath(pos - 1)
    val t = srPath(pos)
    removeFlow(d, s, t, srPath.bw, apply)
    addFlow(d, s, midpoint, srPath.bw, apply)
    addFlow(d, midpoint, t, srPath.bw, apply)
  }

  private def replaceMove(d: Int, pos: Int, midpoint: Int, apply: Boolean): Unit = {
    nMoves += 1
    val srPath = srPaths(d)
    val old = srPath(pos)
    val s = srPath(pos - 1)
    val t = srPath(pos + 1)
    removeFlow(d, s, old, srPath.bw, apply)
    removeFlow(d, old, t, srPath.bw, apply)
    addFlow(d, s, midpoint, srPath.bw, apply)
    addFlow(d, midpoint, t, srPath.bw, apply)
  }

  private def removeMove(d: Int, pos: Int, apply: Boolean): Unit = {
    nMoves += 1
    val srPath = srPaths(d)
    val old = srPath(pos)
    val s = srPath(pos - 1)
    val t = srPath(pos + 1)
    removeFlow(d, s, old, srPath.bw, apply)
    removeFlow(d, old, t, srPath.bw, apply)
    if (s != t) addFlow(d, s, t, srPath.bw, apply)
  }
}

