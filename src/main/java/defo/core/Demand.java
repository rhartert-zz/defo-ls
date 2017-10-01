package defo.core;

public class Demand {
  
  public final int src;
  public final int dest;
  public final double bw;
  
  public Demand(int src, int dest, double bw) {
    this.src = src;
    this.dest = dest;
    this.bw = bw;
  }
}
