package defo.util;

import java.util.Random;

public class CumulativeTree {

  private final int size;
  private double[] weights;
  
  public CumulativeTree(int size) {
    this.size = size;
    this.weights = new double[size * 2];
  }
  
  public double totalWeight() {
    return weights[1];
  }
  
  public void setWeight(int elemId, double weight) {
    int i = size + elemId;
    weights[i] = weight;
    i = i >> 1;
    while (i > 0) {
      final int left = i << 1;
      final int right = left + 1;
      weights[i] = weights[left] + weights[right];
      i = i >> 1;
    }
  }
  
  public int get(Random rng) {
    double w = rng.nextDouble() * weights[1];
    int i = 1;
    while (i < size) {
      final int left = i << 1;
      final int right = left + 1;
      if (w < weights[left]) {
        i = left;
      } else {
        w -= weights[left];
        i = right;
      }
    }
    return i - size;
  }
}
