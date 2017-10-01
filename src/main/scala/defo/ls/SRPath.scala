package defo.ls

import defo.core.Demand

class SRPath(val src: Int, val dest: Int, val bw: Double, maxMidpoints: Int) {
  private[this] val path = new Array[Int](maxMidpoints + 2)
  private[this] var size = 2
  path(0) = src
  path(1) = dest

  def length: Int = size
  
  def apply(i: Int): Int = path(i)
  
  def canInsert: Boolean = size < path.length

  def canRemove: Boolean = size > 2

  def insert(pos: Int, midpoint: Int): Unit = {
    var i = size
    while (i > pos) {
      path(i) = path(i - 1)
      i -= 1
    }
    path(i) = midpoint
    size += 1
  }

  // Compress if removing creates a double midpoint
  def remove(pos: Int): Unit = {
    var j = pos
    var i = pos + 1
    if (path(pos - 1) == path(pos + 1)) i += 1
    
    while (i < size) {
      path(j) = path(i)
      j += 1
      i += 1
    }
    size -= (i - j)
  }

  def substitute(pos: Int, midpoint: Int): Unit = {
    path(pos) = midpoint
  }

  def clear(): Unit = {
    path(1) = dest
    size = 2
  }
  
  override def toString: String = path.take(size).mkString("->")
}

object SRPath {
  def apply(d: Demand, maxMidpoints: Int): SRPath = {
    new SRPath(d.src, d.dest, d.bw, maxMidpoints)
  }
}
