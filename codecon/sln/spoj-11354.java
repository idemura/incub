import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.*;

public class Main {
  static final int INF = 0x7fffffff;

  Main() {
  }

  static void solve() throws Exception {
    long k = in.readLong() - 1;
    // Count digits in 5-6 number.
    int d = 1;
    for (int pot = 2; k >= pot; pot <<= 1) {
      k -= pot;
      d++;
    }
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < d; i++) {
      sb.append((k & 1) == 0? '5': '6');
      k >>= 1;
    }
    out.println(sb.reverse().toString());
  }

  public static void main(String[] args) throws Exception {
    try {
      // System.setIn(new FileInputStream("in"));
      for (int t = in.readInt(); t-- > 0;) {
        solve();
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
