#include "parser.hxx"

namespace igor {
namespace {

int parse_test(string s) {
  int err_count = 0;
  AST ast([&err_count](const string &msg) {
    // cerr<<"Parser error: "<<msg<<endl;
    err_count += 1;
  });

  TempFile temp(s);
  parse(temp.get_name(), &ast);
  return err_count;
}

void test_comment() {
  CHECK(0 == parse_test(
      "# comment till eol\n"
      "# comment"
  ));
}

void test_fn() {
  CHECK(0 == parse_test("fn foo() {}"));
  CHECK(0 == parse_test("fn foo(n: Int) {}"));
  CHECK(0 == parse_test("fn foo(x, y, z: Int) {}"));
  CHECK(0 == parse_test("fn foo(x, y: Int, s: String) {}"));
  CHECK(0 == parse_test("fn foo(): Int {}"));
  CHECK(0 == parse_test("fn foo(x: Int): Int {}"));
  CHECK(0 == parse_test("fn foo(x: Int): x: Int {}"));
  CHECK(0 == parse_test("fn foo(x: Int): x, y: Int {}"));
  CHECK(0 == parse_test("fn foo(x: Int): x: Int, y: Int {}"));
  CHECK(0 == parse_test("fn foo(x: Int): Int, Int {}"));
  // This kind of arg list is accepted by grammar, but possibly should be
  // rejected by semantic analyzer.
  CHECK(0 == parse_test(
      "fn f10(Int) {}\n"
      "fn f11(Int, Int) {}\n"
  ));
  CHECK(1 == parse_test("fn foo(: Int) {}\n"));
  CHECK(1 == parse_test("fn foo(n: Int,) {}\n"));
  CHECK(1 == parse_test("fn foo(n,: Int) {}\n"));
  CHECK(1 == parse_test("fn foo(n: int) {}\n"));
  CHECK(1 == parse_test("fn foo(n:) {}\n"));
  CHECK(1 == parse_test("fn foo(n: Int): {}\n"));
  CHECK(1 == parse_test(
      "fn foo(T) {}\n"
      "fn foo(T) {}\n"
  ));
}

void test_type_spec() {
  // Use that function grammar accepts just type_spec.
  CHECK(0 == parse_test("fn foo(T) {}"));
  CHECK(0 == parse_test("fn foo((T)) {}"));
  CHECK(0 == parse_test("fn foo([T]) {}"));
  CHECK(0 == parse_test("fn foo([T1, T2]) {}"));
  CHECK(0 == parse_test("fn foo(([T1, T2])) {}"));
  CHECK(0 == parse_test("fn foo(T1 => T2) {}"));
  CHECK(0 == parse_test("fn foo(T[]) {}"));
  CHECK(0 == parse_test("fn foo(T1 => T2[]) {}"));
  CHECK(0 == parse_test("fn foo(T1[] => T2) {}"));
  CHECK(0 == parse_test("fn foo([T1, T2] => T[]) {}"));
  CHECK(0 == parse_test("fn foo(T => [T, T] => T[]) {}"));
  CHECK(0 == parse_test("fn foo(T1 => (T2 => T2)[] => (T3 => [T, T])) {}"));
  CHECK(0 == parse_test("fn foo(module.T) {}"));
  CHECK(0 == parse_test("fn foo(module.sub.T) {}"));
  CHECK(0 == parse_test("# big sample:\n"
      "fn foo(f: T => (T => T)[] => (T => [T, T]), n: T):\n"
      "    [T, T, T]\n {"
      "}\n"
  ));
}

void test() {
  CHECK(0 == parse_test(
      "fn f10(x: T) {}\n"
  ));
}

}  // namespace
}  // namespace

int main(int argc, char **argv) {
  using namespace igor;
  std::ios_base::sync_with_stdio(false);
  if (!flags_parse(&argc, argv)) {
    return -1;
  }

  test_comment();
  test_fn();
  test_type_spec();
  test();

  flags_reset();
  RETURN_TESTS_PASSED();
}
