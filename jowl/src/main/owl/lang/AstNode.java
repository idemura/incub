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
  static final AstType EMPTY = new AstType();
  String name;

  @Override
  public void accept(AstVisitor visitor) {
    visitor.visit(this);
  }

  String getReadableName() {
    return name;
  }
}


class AstFunction extends AstBaseNode {
  String name;
  AstArgumentList arguments = AstArgumentList.EMPTY;
  AstType type = AstType.EMPTY;
  AstBlock block;

  @Override
  public void accept(AstVisitor visitor) {
    visitor.visit(this);
  }
}


class AstArgument extends AstBaseNode {
  String name;
  AstType type;

  @Override
  public void accept(AstVisitor visitor) {
    visitor.visit(this);
  }
}


class AstArgumentList extends AstBaseNode {
  static final AstArgumentList EMPTY = new AstArgumentList();
  List<AstArgument> arguments = new ArrayList<>();

  @Override
  public void accept(AstVisitor visitor) {
    visitor.visit(this);
  }

  void add(AstArgument a) {
    arguments.add(a);
  }
}


class AstBlock extends AstBaseNode {
  @Override
  public void accept(AstVisitor visitor) {
    visitor.visit(this);
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
