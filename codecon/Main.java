import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.*;

public class Main {
  static void solve() throws Exception {
  }

  public static void main(String[] args) throws Exception {
    try {
      for (int t = in.readInt(); t > 0; --t) {
        solve();
      }
    } finally {
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
