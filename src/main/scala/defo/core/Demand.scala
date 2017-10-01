package defo.core

class Demand(var src: Int, var dest: Int, var bw: Double)

object Demand {
  def apply(src: Int, dest: Int, bw: Double): Demand = {
    new Demand(src, dest, bw)
  }
}