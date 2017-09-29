package srte.ls

class CumulativeTree(size: Int) {
  
  private[this] val weights = new Array[Double](size*2)
  
  def totalWeight: Double = weights(1)
  
  def changeWeight(value: Int, weight: Double): Unit = { 
    var i = size + value
    weights(i) = weight
    i = i >> 1
    while(i > 0) {     
      val l = i << 1
      val r = l + 1     
      weights(i) = weights(l) + weights(r) 
      i = i >> 1
    }
  }
  
  def get(weight: Double): Int = {
    var i = 1
    var w = weight
    while (i < size) {
      val l = i << 1
      val r = l + 1 
      if (w < weights(l)) i = l
      else {
        w = w - weights(l)
        i = r
      }
    }
    i - size
  }
}

object Test extends App {
  val t = new CumulativeTree(4)
  t.changeWeight(0, 2)
  t.changeWeight(1, 1)
  t.changeWeight(2, 4)
  t.changeWeight(3, 8)
  println(t.totalWeight)
  for (i <- 0 to t.totalWeight.toInt) {
    println(t.get(i))
  }
}