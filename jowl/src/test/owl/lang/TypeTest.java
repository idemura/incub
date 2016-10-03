package owl.lang;

import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class TypeTest {
  private static Type simpleType(String name) {
    Type t = new Type();
    t.name = name;
    return t;
  }

  @Test
  public void testGenericTypeStr() {
    Type t = new Type();
    t.name = "Foo";
    t.params.add(simpleType("I32"));
    t.params.add(simpleType("F32"));
    assertEquals("Foo(I32, F32)", t.typeStr());
  }

  @Test
  public void testTypeStr() {
    Type t = simpleType("Foo");
    assertEquals("Foo", t.typeStr());
  }
}
