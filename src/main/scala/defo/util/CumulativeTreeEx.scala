package defo.util

import scala.collection.mutable.HashMap

class CumulativeTreeEx(initSize: Int) {

  // Tree structure
  private[this] var offset = nextPower2(initSize)
  private[this] var weights = new Array[Double](offset * 2)
  private[this] var values = new Array[Int](offset * 2)
  private[this] var treeSize = 0

  private[this] val hashMap = new HashMap[Int, Int]()

  def totalWeight: Double = weights(1)

  def insert(id: Int, weight: Double): Unit = {
    if (!hashMap.contains(id)) {
      val n = offset + treeSize
      if (n == weights.length) rescale()
      weights(n) = weight
      values(n) = id
      propagate(n)
      hashMap.put(id, treeSize)
      treeSize += 1
    }
  }
  
  def keys = hashMap.keys
  
  def iterator = hashMap.keys.iterator

  def changeWeight(id: Int, weight: Double): Unit = {
    val n = offset + hashMap.apply(id)
    weights(n) = weight
    propagate(n)
  }

  def remove(id: Int): Unit = {
    if (hashMap.contains(id)) {
      treeSize -= 1
      val n = offset + hashMap.apply(id)
      val lastN = offset + treeSize
      weights(n) = weights(lastN)
      values(n) = values(lastN)
      propagate(lastN)
      propagate(n)
      hashMap.remove(id)
    }
  }

  def get(weight: Double): Int = {
    var i = 1
    var w = weight
    while (i < offset) {
      val l = i << 1
      val r = l + 1
      if (w < weights(l)) i = l
      else {
        w = w - weights(l)
        i = r
      }
    }
    values(i)
  }
  
  def contains(e: Int): Boolean = hashMap.contains(e)

  def clear(): Unit = {
    hashMap.clear()
    treeSize = 0
  }

  @inline private def propagate(n: Int): Unit = {
    var p = n >> 1
    while (p > 0) {
      val l = p << 1
      val r = l + 1
      weights(p) = weights(l) + weights(r)
      p = p >> 1
    }
  }

  @inline private def nextPower2(n: Int): Int = {
    var i = n
    i |= i >> 1
    i |= i >> 2
    i |= i >> 4
    i |= i >> 8
    i |= i >> 16
    i + 1
  }

  @inline private def rescale(): Unit = {
    val oldSize = weights.length
    val newWeights = new Array[Double](oldSize * 2)
    val newValues = new Array[Int](oldSize * 2)
    System.arraycopy(weights, offset, newWeights, oldSize, offset)
    System.arraycopy(values, offset, newValues, oldSize, offset)
    weights = newWeights
    values = newValues
    offset = oldSize
    // Recompute internal nodes
    var p = offset
    while (p > 1) {
      p -= 1
      val l = p << 1
      val r = l + 1
      weights(p) = weights(l) + weights(r)
    }
  }
}