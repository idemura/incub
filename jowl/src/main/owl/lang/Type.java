package owl.lang;

import java.util.ArrayList;
import java.util.List;

// Generic type with parameters.
public class Type {
  String name;
  List<Type> params = new ArrayList<>();

  public final String typeStr() {
    String s = name;
    if (params != null && params.size() > 0) {
      s += "(";
      s += params.get(0).typeStr();
      for (int i = 1; i < params.size(); i++) {
        s += ", ";
        s += params.get(i).typeStr();
      }
      s += ")";
    }
    return s;
  }
}
