package srte.util

import scala.collection.mutable.ArrayBuffer
import srte.island.Island

class Instance(val topology: Topology, val weights: Array[Int], val capacities: Array[Double], val demands: Array[Demand], val lb: Double)

object Instance {  
  
  private def parseRaw(topologyFile: String, demandsFile: String): Instance = {
    val topologyData = TopologyParser.parse(topologyFile)
    val demandsData = DemandParser.parse(demandsFile)
    val topology = Topology(topologyData.edgeSrcs, topologyData.edgeDests)
    val capacities = topologyData.edgeCapacities.map(_.toDouble)
    val demands = ArrayBuffer[Demand]()
    demandsData.Demands.foreach(demand => {
      val src = demandsData.demandSrcs(demand)
      val dest = demandsData.demandDests(demand)
      val traffic = demandsData.demandTraffics(demand)
      demands.append(new Demand(src, dest, traffic))
    })
    new Instance(topology, topologyData.edgeWeights, capacities, demands.toArray, 0.0)
  }
  
  def parse(topologyFile: String, demandsFile: String, reduce: Boolean = false): Instance = {
    val instance = parseRaw(topologyFile, demandsFile)
    if (!reduce) return instance
    else return Island.reduction(instance)
  }
}