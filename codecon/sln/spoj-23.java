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
    int ab = in.readInt();
    int ac = in.readInt();
    int ad = in.readInt();
    int bc = in.readInt();
    int bd = in.readInt();
    int cd = in.readInt();
    double U = ab, V = bc, W = ac;
    double u = cd, v = ad, w = bd;
    double X = (v + w + U) * (v + w - U);
    double x = (U - v + w) * (U + v - w);
    double Y = (u + w + V) * (u + w - V);
    double y = (V - u + w) * (V + u - w);
    double Z = (u + v + W) * (u + v - W);
    double z = (W - u + v) * (W + u - v);
    double a = Math.sqrt(x * Y * Z);
    double b = Math.sqrt(y * Z * X);
    double c = Math.sqrt(z * X * Y);
    double d = Math.sqrt(x * y * z);
    double volume =  Math.sqrt((b + c + d - a) * (a + c + d - b) *
                               (a + b + d - c) * (a + b + c - d))
                     / (192.0 * u * v * w);
    out.printf("%.4f\n", volume);
  }

  public static void main(String[] args) throws Exception {
    try {
      // System.setIn(new FileInputStream("in"));
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
