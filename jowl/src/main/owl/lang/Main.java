package owl.lang;

public class Main {
  public static void main(String[] args) {
    System.out.println("hello");
    SimpleCharStream reader = new SimpleCharStream(System.in);
    Parser parser = new Parser(new ParserTokenManager(reader));
    System.out.println("new version");
  }
}
