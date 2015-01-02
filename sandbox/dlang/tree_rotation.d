import std.algorithm, std.conv, std.random, std.stdio;

// TODO: Predicate.
final class Tree(T) {
public:
  alias T KeyT;

  this() {
  }

  KeyT[] inorder() {
    KeyT[] res;
    void rec(Node* n) {
      if (n == null) {
        return;
      }
      rec(n.l);
      res ~= n.key;
      rec(n.r);
    }
    rec(root);
    return res;
  }

  void insert(KeyT key) {
    if (root == null) {
      root = new Node;
      root.key = key;
      return;
    }

    Node* newNode;
    auto p = root;
    while (p) {
      if (key <= p.key) {
        if (p.l) {
          p = p.l;
        } else {
          p.l = newNode = new Node;
          break;
        }
      } else {
        if (p.r) {
          p = p.r;
        } else {
          p.r = newNode = new Node;
          break;
        }
      }
    }
    newNode.key = key;
    newNode.p = p;
  }

  Node* getNode(KeyT key) {
    auto p = root;
    while (p) {
      if (p.key == key) {
        return p;
      }
      if (key <= p.key) {
        p = p.l;
      } else {
        p = p.r;
      }
    }
    return null;
  }

  void print() {
    void rec(Node* node, string tab) {
      if (!node) {
        writefln("%snull", tab);
        return;
      }
      writefln("%s%x %s l %x r %x p %x", tab, node, node.key,
               node.l, node.r, node.p);
      rec(node.l, tab ~ "  ");
      rec(node.r, tab ~ "  ");
    }
    rec(root, "");
  }

  void rotate(Node* n) {
    if (n == null || n.p == null) {
      return;
    }
    auto m = n.p;
    if (m.p) {
      if (m.p.l == m) {
        m.p.l = n;
      } else {
        m.p.r = n;
      }
    } else {
      assert(root == m);
      root = n;
    }
    n.p = m.p;
    m.p = n;
    if (m.l == n) {
      m.l = n.r;
      if (m.l) m.l.p = m;
      n.r = m;
    } else {
      m.r = n.l;
      if (m.r) m.r.p = m;
      n.l = m;
    }
  }

private:
  static struct Node {
    KeyT key;
    Node* l, r, p;
  }

  Node* root;
}

void testRotation(T)(Tree!T t, T key) {
  writefln("Rotate %s", key);
  t.rotate(t.getNode(key));
  t.print();
  writefln("%s", t.inorder());
}

void main(string[] args) {
  auto t = new Tree!int;
  t.insert(10);
  t.insert(5);
  t.insert(8);
  t.insert(9);
  t.insert(20);
  t.insert(15);
  t.insert(25);
  t.print();
  writefln("%s", t.inorder());
  t.testRotation(8);
  t.testRotation(15);
  t.testRotation(8);
}
