import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.*;

public class Main {
  static final int INF = 0x7fffffff;

  Main() {
  }

  void solve() throws Exception {
    int n = in.readInt();
    // Approach is the same as matrix multiplication order: split i..k range at
    // some index j: i<=j<=k.
    int[][] smoke = new int[n][n];
    int[][] color = new int[n][n];
    // Diagonal initialized with zeros by operator new.
    for (int i = 0; i < n; i++) {
      color[i][i] = in.readInt();
    }
    // `i` is length of a array (subproblem size).
    for (int i = 1; i < n; i++) {
      // `j` is starting index of the array.
      for (int j = 0; j + i < n; j++) {
        int minSmoke = INF, minColor = -1;
        // `k` is position of split on range [j, j + i).
        for (int k = 0; k < i; k++) {
          int m = j + k;
          int s = smoke[j][m] + smoke[m + 1][j + i] +
                  color[j][m] * color[m + 1][j + i];
          if (s < minSmoke) {
            minSmoke = s;
            minColor = (color[j][m] + color[m + 1][j + i]) % 100;
          }
        }
        smoke[j][j + i] = minSmoke;
        color[j][j + i] = minColor;
      }
    }
    int answer = smoke[0][n - 1];
    out.printf("%d\n", answer);
  }

  public static void main(String[] args) throws Exception {
    try {
      // System.setIn(new FileInputStream("in"));
      for (; !in.isEof();) {
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
