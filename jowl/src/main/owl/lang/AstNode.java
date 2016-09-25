package owl.lang;

import java.util.ArrayList;
import java.util.List;

public interface AstNode {
  public void addChild(AstNode node);
  public String getClassName();
  public String debugString();
  public void printTreeDebug();
}

abstract class BaseNode
    implements AstNode {
  private List<AstNode> child;

  @Override
  public void addChild(AstNode node) {
    if (child == null) {
      child = new ArrayList<AstNode>();
    }
    child.add(node);
  }

  @Override
  public String getClassName() {
    String fullName = this.getClass().getName();
    return fullName.substring(fullName.lastIndexOf('.') + 1);
  }

  @Override
  public void printTreeDebug() {
    printDebugStep(this, 0);
  }

  @Override
  public String debugString() {
    return getClassName();
  }

  private static void printDebugStep(BaseNode node, int tab) {
    printTabs(tab);
    System.out.print(node.debugString());
    if (node.child == null) {
      System.out.println(";");
    } else {
      System.out.println(" {");
      for (AstNode c : node.child) {
        printDebugStep((BaseNode) c, tab + 1);
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

class NameNode extends BaseNode {
  private String name;

  public NameNode(String name) {
    this.name = name;
  }

  @Override
  public String debugString() {
    return getClassName() + "(" + name + ")";
  }
}

class ModuleNode extends BaseNode {
}
