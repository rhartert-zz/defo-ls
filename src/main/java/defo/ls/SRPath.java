package defo.ls;

public class SRPath {
  
  private int[] path;
  private int length;
  
  public SRPath(int src, int dest, int maxMidpoints) {
    this.path = new int[maxMidpoints + 2];
    this.length = 2;
    path[0] = src;
    path[1] = dest;
  }
  
  public int length() {
    return length;
  }
  
  public int src() {
    return path[0];
  }
  
  public int dest() {
    return path[length - 1];
  }
  
  public int get(int position) {
    return path[position];
  }
  
  public void insert(int position, int midpoint) {
    for (int i = length; i > position; i--) {
      path[i] = path[i - 1];
    }
    path[position] = midpoint;
    length++;
  }
  
  public void remove(int position) {
    int j = position;
    int i = j;
    if (path[position - 1] == path[position + 1]) {
      i++;
    }
    while (i < length) {
      path[j] = path[i];
      j++;
      i++;
    }
    length -= (i-j);
  }
  
  public void substitute(int position, int midpoint) {
    path[position] = midpoint;
  }
  
  public void clear() {
    path[1] = path[length - 1];
    length = 2;
  }
  
  @Override
  public String toString() {
    StringBuffer bf = new StringBuffer();
    for (int i = 0; i < length - 1; i++) {
      bf.append(path[i] + "->");
    }
    bf.append(path[length - 1]);
    return bf.toString();
  }
}
