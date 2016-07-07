#include "parser.hxx"

namespace igor {
namespace {

int parse_test(string s) {
  ErrorSink es({});
  AST ast(&es);

  TempFile temp(s);
  parse(temp.get_name(), &ast);
  return es.err_count();
}

void test_comment() {
  CHECK(0 == parse_test(
      "# comment till eol\n"
      "# comment"
  ));
}

void test_fn() {
  // Grammar accepts this. But semantic checker should error on it.
  CHECK(0 == parse_test("fn foo(x) {}"));
  CHECK(0 == parse_test("fn foo(x, y, z) {}"));

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
  // Type only in args:
  CHECK(0 == parse_test("fn foo(Int) {}\n"));
  CHECK(0 == parse_test("fn foo(Int, Int) {}\n"));
  CHECK(0 == parse_test("fn foo(Int => Int) {}\n"));
  CHECK(0 == parse_test("fn foo((Int)) {}\n"));

  CHECK(1 == parse_test("fn foo(: Int) {}\n"));
  CHECK(1 == parse_test("fn foo(n: Int,) {}\n"));
  CHECK(1 == parse_test("fn foo(n,: Int) {}\n"));
  CHECK(1 == parse_test("fn foo(n: int) {}\n"));
  CHECK(1 == parse_test("fn foo(n:) {}\n"));
  CHECK(1 == parse_test("fn foo(n: Int): {}\n"));
  // Grammar accepts same function names.
  CHECK(0 == parse_test(
      "fn foo(T) {}\n"
      "fn foo(T) {}\n"
  ));
}

void test_type() {
  // Use that function grammar accepts just type_spec.
  CHECK(0 == parse_test("fn foo(x: T) {}"));
  CHECK(0 == parse_test("fn foo(x: (T)) {}"));
  CHECK(0 == parse_test("fn foo(x: [T]) {}"));
  CHECK(0 == parse_test("fn foo(x: [T1, T2]) {}"));
  CHECK(0 == parse_test("fn foo(x: ([T1, T2])) {}"));
  CHECK(0 == parse_test("fn foo(x: T1 => T2) {}"));
  CHECK(0 == parse_test("fn foo(x: T[]) {}"));
  CHECK(0 == parse_test("fn foo(x: T1 => T2[]) {}"));
  CHECK(0 == parse_test("fn foo(x: T1[] => T2) {}"));
  CHECK(0 == parse_test("fn foo(x: [T1, T2] => T[]) {}"));
  CHECK(0 == parse_test("fn foo(x: T => [T, T] => T[]) {}"));
  CHECK(0 == parse_test("fn foo(x: T1 => (T2 => T2)[] => (T3 => [T, T])) {}"));
  CHECK(0 == parse_test("fn foo(x: module.T) {}"));
  CHECK(0 == parse_test("fn foo(x: module.sub.T) {}"));
  CHECK(0 == parse_test(
      "# big sample:\n"
      "fn foo(f: T => (T => T)[] => (T => [T, T]), n: T):\n"
      "    [T, T, T]\n {"
      "}\n"
  ));
}

void semantic_fn() {
  string s = "fn foo(x) {}";
  std::stringstream ss;
  ErrorSink::Args args;
  args.report_location = false;
  args.stream = &ss;
  ErrorSink es(args);
  AST ast(&es);

  TempFile temp(s);
  CHECK(parse(temp.get_name(), &ast));
  CHECK(0 == es.err_count());
  CHECK(ast.analyze_semantic());
  CHECK(0 == es.err_count());
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
  test_type();

  semantic_fn();

  {
    CHECK(0 == parse_test(
        "fn foo(x) {}\n"
    ));
  }

  flags_reset();
  RETURN_TESTS_PASSED();
}
