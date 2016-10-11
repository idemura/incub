package owl.lang;

import java.util.ArrayList;
import java.util.List;


public class Ast {
    AstNode root;
}


interface AstNode {
    static interface Visitor {
        //void visit(AstName node) {}
        void visit(AstModule node);
        void visit(AstFunction node);
        void visit(AstVariable node);
        void visit(AstBlock node);
    }

    void accept(Visitor visitor);
}


abstract class AstBaseNode implements AstNode {
}


// class AstName extends AstBaseNode {
//     static AstName WILDCARD = new AstName();
//     static {
//         WILDCARD.name = "_";
//     }
//
//     String name;
//
//     @Override
//     public void accept(AstNode.Visitor v) {
//         v.visit(this);
//     }
//
//     final boolean isWildCard() {
//         return this == WILDCARD;
//     }
// }


class AstModule extends AstBaseNode {
    List<AstFunction> functions = new ArrayList<>();

    @Override
    public void accept(AstNode.Visitor visitor) {
        visitor.visit(this);
    }

    final void addFunction(AstFunction f) {
        functions.add(f);
    }
}


class AstFunction extends AstBaseNode {
    String name;
    List<AstVariable> arguments = new ArrayList<>();
    List<AstVariable> returns = new ArrayList<>();
    AstBlock block;

    @Override
    public void accept(AstNode.Visitor v) {
        v.visit(this);
    }
}


class AstVariable extends AstBaseNode {
    String name;
    Type type;

    @Override
    public void accept(AstNode.Visitor v) {
        v.visit(this);
    }
}


class AstBlock extends AstBaseNode {
    @Override
    public void accept(AstNode.Visitor v) {
        v.visit(this);
    }
}
