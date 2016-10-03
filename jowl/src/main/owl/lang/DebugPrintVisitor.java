package owl.lang;

class DebugPrintVisitor
    implements AstNode.Visitor {
  private int tab = 0;

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
    node("arguments");
    for (AstVariable a : n.arguments) {
      a.accept(this);
    }
    nodeDone();
    node("returns");
    for (AstVariable a : n.returns) {
      a.accept(this);
    }
    nodeDone();
    n.block.accept(this);
    nodeDone();
  }

  @Override
  public void visit(AstVariable n) {
    node(n);
    if (n.name != null) {
      prop("name", n.name);
    }
    if (n.type != null) {
      prop("type", n.type.typeStr());
    }
    nodeDone();
  }

  @Override
  public void visit(AstBlock n) {
    leaf(n, null);
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

  private void node(String text) {
    print(text + " {");
    tab++;
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
