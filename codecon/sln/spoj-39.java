import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.*;

public class Main {
  static final int INF = 0x7fffffff;

  // Index - weight, value - accumulated price.
  int[] dp = new int[10002];
  int[] cp = new int[502];  // P - values.
  int[] cw = new int[502];  // W - weights.

  Main() {
  }

  int solveInFn() throws Exception {
    // Solution is exactly like knapsack problem, only difference that after we
    // put a coin, we should still try it again and again.
    int e = in.readInt(), f = in.readInt();
    f -= e;
    if (f == 0) {
      return 0;
    }
    int n = in.readInt();
    for (int i = 0; i < n; i++) {
      cp[i] = in.readInt();
      cw[i] = in.readInt();
    }
    for (int i = 0; i < f; i++) {
      if (i > 0 && dp[i] == 0) {
        continue;
      }
      for (int j = 0; j < n; j++) {
        int wj = i + cw[j];
        if (wj > f) {
          continue;
        }
        int pj = dp[i] + cp[j];
        if (dp[wj] == 0 || pj < dp[wj]) {
          dp[wj] = pj;
        }
      }
    }
    return dp[f] == 0? -1: dp[f];
  }

  void solve() throws Exception {
    int res = solveInFn();
    if (res < 0) {
      out.println("This is impossible.");
    } else {
      out.printf("The minimum amount of money in the piggy-bank is %d.\n", res);
    }
  }

  public static void main(String[] args) throws Exception {
    try {
      for (int t = in.readInt(); t-- > 0;) {
        new Main().solve();
      }
    } finally {
      in.close();
      out.close();
    }
  }

  private static StdInTokenizer in = new StdInTokenizer();
  private static PrintWriter out = new PrintWriter(System.out);

  public static class StdInTokenizer {
    public StdInTokenizer() {
      reader = new BufferedReader(new InputStreamReader(System.in));
    }

    public void close() {
      try {
        reader.close();
      } catch (Exception e) {
      }
    }

    public int readInt() throws Exception {
      return Integer.parseInt(readString());
    }

    public long readLong() throws Exception {
      return Long.parseLong(readString());
    }

    public char readChar() throws Exception {
      throw new Exception("Not implemented!");
    }

    public String readString() throws Exception {
      if (!readNextLine()) {
        throw new Exception("StdIn EOF");
      }
      return tokens[tokenIndex++];
    }

    public double readDouble() throws Exception {
      return Double.parseDouble(readString());
    }

    public boolean isEof() throws Exception {
      return !readNextLine();
    }

    private boolean readNextLine() throws Exception {
      if (!eof && tokenIndex == tokens.length) {
        String s = reader.readLine();
        if (s == null) {
          eof = true;
        } else {
          tokens = s.split("\\s+");
          tokenIndex = 0;
        }
      }
      return !eof;
    }

    private BufferedReader reader;
    private String[] tokens = new String[0];
    private int tokenIndex;
    private boolean eof = false;
  }
}
