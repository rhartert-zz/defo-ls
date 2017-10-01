package defo.ls;

public class NetworkState {
  
  // Quantity of flow forwarded on each link.
  private double[] flows;
  
  // Keep track of the changes since the last saved state.
  private double[] changedFlows;
  private int[] changedLinks;
  private boolean[] changed;
  private int nChanges;
  
  public NetworkState(int nLinks) {
    this.flows = new double[nLinks];
    this.changedFlows = new double[nLinks];
    this.changedLinks = new int[nLinks];
    this.changed = new boolean[nLinks];
    this.nChanges = 0;
  }
  
  /** Returns the quantity of data forwarded on `linkId`. */
  public double flow(int linkId) {
    return flows[linkId];
  }
  
  public int[] changes() {
    return changedLinks;
  }
  
  public int nChanges() {
    return nChanges;
  }
  
  /** Saves the current changes. */
  public void saveChanges() {
    while (nChanges > 0) {
      nChanges--;
      changed[changedLinks[nChanges]] = false;
    }
  }

  /** Undoes the changes to restore the last saved state. */
  public void undoChanges() {
    while (nChanges > 0) {
      nChanges--;
      int linkId = changedLinks[nChanges];
      changed[linkId] = false;
      flows[linkId] = changedFlows[linkId];
    }
  }

  public void addFlow(int linkId, double flow) {
    if (!changed[linkId]) {
      changed[linkId] = true;
      changedFlows[linkId] = flows[linkId];
      changedLinks[nChanges] = linkId;
      nChanges++;
    }
    flows[linkId] += flow;
  }
  
  public void removeFlow(int linkId, double flow) {
    addFlow(linkId, -flow);
  }
}
