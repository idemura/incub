package owl.lang;

import java.lang.Iterable;
import java.util.ArrayList;
import java.util.List;

public interface AstNode {
  // public void addChild(AstNode node);
  public Iterable<AstNode> getChildren();
  public String getClassName();
  public String debugString();
  public void debugPrint();
}


abstract class AstBaseNode
    implements AstNode {
  // private List<AstNode> child;
  //
  // public void addChild(AstNode node) {
  //   if (child == null) {
  //     child = new ArrayList<AstNode>();
  //   }
  //   child.add(node);
  // }

  @Override
  public String getClassName() {
    String fullName = this.getClass().getName();
    return fullName.substring(fullName.lastIndexOf('.') + 1);
  }

  @Override
  public void debugPrint() {
    printDebugStep(this, 0);
  }

  @Override
  public String debugString() {
    String details = debugStringDetail();
    if (details != null) {
      return getClassName() + "(" + details + ")";
    } else {
      return getClassName();
    }
  }

  protected String debugStringDetail() {
    return null;
  }

  private static void printDebugStep(AstBaseNode node, int tab) {
    printTabs(tab);
    System.out.print(node.debugString());
    if (node.child == null) {
      System.out.println(";");
    } else {
      System.out.println(" {");
      for (AstNode c : node.child) {
        printDebugStep((AstBaseNode) c, tab + 1);
      }
      printTabs(tab);
      System.out.println("}");
    }
  }

  private static void printTabs(int tab) {
    for (int i = 0; i < tab; i++) {
      System.out.print("  ");
    }
  }
}


class AstName extends AstBaseNode {
  static AstName WILDCARD = new AstName("_");

  private String name;

  AstName(String name) {
    this.name = name;
  }

  @Override
  Iterable<AstNode> getChildren() {
    return null;
  }

  @Override
  protected String debugStringDetail() {
    return name;
  }

  final void join(String subname) {
    name += "." + subname;
  }

  final boolean isWildCard() {
    return this == WILDCARD;
  }
}


class AstFunction extends AstBaseNode {
  private String name;

  AstFunction(String name) {
    this.name = name;
  }

  @Override
  protected String debugStringDetail() {
    return name;
  }
}

class AstModule extends AstBaseNode {
  private List<AstFunction> functions = new ArrayList<AstFunction>();

  final void addFunction(AstFunction fn) {
    functions.add(fn);
  }

  @Override
  String debugStringDetail() {}
}
