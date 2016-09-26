package owl.lang;

import java.util.ArrayList;
import java.util.List;

public interface AstNode {
  void accept(AstVisitor visitor);
}


abstract class AstBaseNode
    implements AstNode {
}


class AstName extends AstBaseNode {
  static AstName WILDCARD = new AstName();
  static {
    WILDCARD.name = "_";
  }
  
  String name;

  @Override
  public void accept(AstVisitor visitor) {
    visitor.visit(this);
  }

  final boolean isWildCard() {
    return this == WILDCARD;
  }
}


class AstType extends AstBaseNode {
  String name;

  @Override
  public void accept(AstVisitor visitor) {
    visitor.visit(this);
  }
}


class AstFunction extends AstBaseNode {
  String name;
  AstArgumentList arguments;

  @Override
  public void accept(AstVisitor visitor) {
    visitor.visit(this);
  }
}


class AstArgument extends AstBaseNode {
  String name;

  @Override
  public void accept(AstVisitor visitor) {
    visitor.visit(this);
  }
}


class AstArgumentList extends AstBaseNode {
  List<AstArgument> arguments = new ArrayList<>();

  @Override
  public void accept(AstVisitor visitor) {
    visitor.visit(this);
  }

  void addArgument(AstArgument a) {
    arguments.add(a);
  }
}


class AstModule extends AstBaseNode {
  List<AstFunction> functions = new ArrayList<>();

  @Override
  public void accept(AstVisitor visitor) {
    visitor.visit(this);
  }

  final void addFunction(AstFunction f) {
    functions.add(f);
  }
}
