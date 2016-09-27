package owl.lang;

public interface AstVisitor {
  void visit(AstName node);
  void visit(AstModule node);
  void visit(AstFunction node);
  void visit(AstArgument node);
  void visit(AstArgumentList node);
  void visit(AstBlock node);
  void visit(AstType node);
}


class DebugPrintVisitor
    implements AstVisitor {
  private int tab = 0;

  @Override
  public void visit(AstName n) {
    leaf(n, n.name);
  }

  @Override
  public void visit(AstModule n) {
    node(n);
    for (AstFunction f : n.functions) {
      f.accept(this);
    }
    nodeDone();
  }

  @Override
  public void visit(AstFunction n) {
    node(n);
    prop("name", n.name);
    n.arguments.accept(this);
    n.outputs.accept(this);
    n.block.accept(this);
    nodeDone();
  }

  @Override
  public void visit(AstArgument n) {
    node(n);
    if (n.name != null) {
      prop("name", n.name);
    }
    if (n.type != null) {
      prop("type", n.type.getReadableName());
    }
    nodeDone();
  }

  @Override
  public void visit(AstArgumentList n) {
    node(n);
    for (AstArgument a : n.arguments) {
      a.accept(this);
    }
    nodeDone();
  }

  @Override
  public void visit(AstBlock n) {
    leaf(n, null);
  }

  @Override
  public void visit(AstType n) {
    leaf(n, n.name);
  }

  private void prop(String name, String s) {
    print(name + ": " + s);
  }

  private void leaf(AstNode node, String s) {
    String text = getClassName(node);
    if (s != null) {
      text += " " + s;
    }
    print(text + ";");
  }

  private void node(AstNode node) {
    print(getClassName(node) + " {");
    tab++;
  }

  private void nodeDone() {
    tab--;
    print("}");
  }

  private static String getClassName(AstNode node) {
    String fullName = node.getClass().getName();
    return fullName.substring(fullName.lastIndexOf('.') + 1);
  }

  private void print(String s) {
    for (int i = 0; i < tab; i++) {
      System.out.print("  ");
    }
    System.out.println(s);
  }
}
