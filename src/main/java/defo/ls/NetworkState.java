package defo.ls;

public class NetworkState {
  
  // Quantity of flow forwarded on each link.
  private double[] flows;
  
  // States.
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
  
  public double flow(int linkId) {
    return flows[linkId];
  }
  
  public int[] changes() {
    return changedLinks;
  }
  
  public int nChanges() {
    return nChanges;
  }
  
  // Save the current changes so that the current solution becomes the default
  // solution. 
  public void saveChanges() {
    while (nChanges > 0) {
      nChanges--;
      changed[changedLinks[nChanges]] = false;
    }
  }

  // Undo the last changes so that the default solution is restored.
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
