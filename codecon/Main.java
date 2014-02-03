import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.*;

public class Main {
  // Values of sequence we know.
  int[] vs;

  Main() {
  }

  static int gcd(int a, int b) {
    if (a < 0) a = -a;
    if (b < 0) b = -b;
    while (b != 0) {
      int t = a % b;
      a = b;
      b = t;
    }
    return a;
  }

  static int mulDiv(int x, int m, int d) {
    return (int)((long)x * m / d);
  }

  int extrapolate(int x) {
    final int kMax = 20000000;
    int s = 0, n = 1, d = 1;
    // Skip first because it will zero `k`.
    for (int i = 1; i < vs.length; i++) {
      n *= x - i - 1;
      d *= -i;
      if (n > kMax) {
        int g = gcd(n, d);
        n /= g;
        d /= g;
      }
    }
    int k = n / d;
    // Now we can do incrementally: add summand, modify coefficient.
    for (int i = 0; ; i++) {
      s += k * vs[i];
      // We know that `md` is a divisor of `k`, so can divide and than multiply
      // without concerns about overflow or remainders.
      int xi = x - i - 1;
      int ii = i + 1;
      if (ii == vs.length) break;
      k = mulDiv(mulDiv(k, xi, xi - 1), ii - vs.length, ii);
    }
    return s;
  }

  // Let y1,y2,y3...yn our series. We can imagine it as function on
  // x1=1,x2=2,x3=3...xn=n: y1=p(x1),...,yn=p(xn). Use this formula to get
  // polynomial such that yi=p(xi), xi=x1,x2...xn:
  //  p(x) = y1(x-x2)(x-x3)/(x1-x2)/(x1-x3) +
  //         (x-x1)y2(x-x3)/(x2-x1)/(x2-x3) +
  //         (x-x1)(x-x2)y3/(x3-x1)/(x3-x2);
  // Since xi=i, we transform the formula:
  //  p(x) = y1(x-2)(x-3)/(1-2)/(1-3) +
  //         (x-1)y2(x-3)/(2-1)/(2-3) +
  //         (x-1)(x-2)y3/(3-1)/(3-2);
  // After some time thinking and checking on computer, I can see that every
  // summand is integer, so I don't need rationals.
  void solve() throws Exception {
    int s = in.readInt(), c = in.readInt();
    vs = new int[s];
    for (int i = 0; i < vs.length; i++) {
      vs[i] = in.readInt();
    }
    for (int i = 1; i <= c; i++) {
      int e = extrapolate(i + s);
      out.print(e + " ");
    }
    out.println();
  }

  public static void main(String[] args) throws Exception {
    try {
      for (int t = in.readInt(); t > 0; --t) {
        new Main().solve();
      }
    } finally {
      in.close();
      out.close();
    }
  }

  private static StdInputScanner in = new StdInputScanner();
  private static PrintWriter out = new PrintWriter(System.out);

  static class StdInputScanner {
    public StdInputScanner() {
      reader = new BufferedReader(new InputStreamReader(System.in));
    }

    public void close() throws Exception {
      reader.close();
    }

    public int readInt() throws Exception {
      return Integer.parseInt(readWord());
    }

    public String readString() throws Exception {
      return readWord();
    }

    public double readDouble() throws Exception {
      return Double.parseDouble(readWord());
    }

    public boolean isEof() {
      return eof;
    }

    private String readWord() throws Exception {
      if (wordIndex == words.length) {
        readNextLine();
      }
      if (eof) {
        throw new Exception("StdIn EOF");
      }
      return words[wordIndex++];
    }

    private void readNextLine() throws Exception {
      if (eof) {
        return;
      }
      String s = reader.readLine();
      if (s == null) {
        eof = true;
        return;
      }
      words = s.split("\\s+");
      wordIndex = 0;
    }

    private BufferedReader reader;
    private String[] words = new String[0];
    private int wordIndex;
    private boolean eof;
  }
}
