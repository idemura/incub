#include "parser.hxx"

namespace igor {
namespace {

void report(const ErrorSink &es, int expected_errors, const string &code) {
  cerr<<"Test code:\n"<<code
      <<"\nexpected errors: "<<expected_errors
      <<"\nactual: "<<es.err_count()<<"\n";
  if (es.err_count() > 0) es.print_to_stderr(false);
}

bool parse_test(int expected_errors, const string &code) {
  ErrorSink es("<test>");
  AST ast(&es);
  TempFile temp(code);
  parse(temp.get_name(), &ast);
  if (expected_errors != es.err_count()) {
    report(es, expected_errors, code);
    return false;
  }
  return true;
}

void test_comment() {
  CHECK(parse_test(0,
      "# comment till eol\n"
      "# comment"
  ));
}

void test_fn() {
  // Grammar accepts this. But semantic checker should error on it.
  CHECK(parse_test(0, "fn foo(x) {}"));
  CHECK(parse_test(0, "fn foo(x, y, z) {}"));

  CHECK(parse_test(0, "fn foo() {}"));
  CHECK(parse_test(0, "fn foo(n: Int) {}"));
  CHECK(parse_test(0, "fn foo(x, y, z: Int) {}"));
  CHECK(parse_test(0, "fn foo(x, y: Int, s: String) {}"));
  CHECK(parse_test(0, "fn foo(): Int {}"));
  CHECK(parse_test(0, "fn foo(x: Int): Int {}"));
  CHECK(parse_test(0, "fn foo(x: Int): x: Int {}"));
  CHECK(parse_test(0, "fn foo(x: Int): x, y: Int {}"));
  CHECK(parse_test(0, "fn foo(x: Int): x: Int, y: Int {}"));
  CHECK(parse_test(0, "fn foo(x: Int): Int, Int {}"));
  // Type only in args:
  CHECK(parse_test(0, "fn foo(Int) {}\n"));
  CHECK(parse_test(0, "fn foo(Int, Int) {}\n"));
  CHECK(parse_test(0, "fn foo(Int => Int) {}\n"));
  CHECK(parse_test(0, "fn foo((Int)) {}\n"));

  CHECK(parse_test(1, "fn foo(: Int) {}\n"));
  CHECK(parse_test(1, "fn foo(n: Int,) {}\n"));
  CHECK(parse_test(1, "fn foo(n,: Int) {}\n"));
  CHECK(parse_test(1, "fn foo(n: int) {}\n"));
  CHECK(parse_test(1, "fn foo(n:) {}\n"));
  CHECK(parse_test(1, "fn foo(n: Int): {}\n"));
  // Grammar accepts same function names.
  CHECK(parse_test(0,
      "fn foo(T) {}\n"
      "fn foo(T) {}\n"
  ));
}

void test_type() {
  // Use that function grammar accepts just type_spec.
  CHECK(parse_test(0, "fn foo(x: T) {}"));
  CHECK(parse_test(0, "fn foo(x: (T)) {}"));
  CHECK(parse_test(0, "fn foo(x: [T]) {}"));
  CHECK(parse_test(0, "fn foo(x: [T1, T2]) {}"));
  CHECK(parse_test(0, "fn foo(x: ([T1, T2])) {}"));
  CHECK(parse_test(0, "fn foo(x: T1 => T2) {}"));
  CHECK(parse_test(0, "fn foo(x: T[]) {}"));
  CHECK(parse_test(0, "fn foo(x: T1 => T2[]) {}"));
  CHECK(parse_test(0, "fn foo(x: T1[] => T2) {}"));
  CHECK(parse_test(0, "fn foo(x: [T1, T2] => T[]) {}"));
  CHECK(parse_test(0, "fn foo(x: T => [T, T] => T[]) {}"));
  CHECK(parse_test(0, "fn foo(x: T1 => (T2 => T2)[] => (T3 => [T, T])) {}"));
  CHECK(parse_test(0, "fn foo(x: module.T) {}"));
  CHECK(parse_test(0, "fn foo(x: module.sub.T) {}"));
  CHECK(parse_test(0,
      "# big sample:\n"
      "fn foo(f: T => (T => T)[] => (T => [T, T]), n: T):\n"
      "    [T, T, T]\n {"
      "}\n"
  ));
}

bool semantic_test(const string &expected_msg, const string &code) {
  ErrorSink es("<test>");
  AST ast(&es);
  TempFile temp(code);
  CHECK(parse(temp.get_name(), &ast));
  ast.analyze_semantic();
  if (expected_msg.empty() != (0 == es.err_count())) {
    report(es, 1, code);
    return false;
  }
  if (expected_msg.empty()) {
    return true;
  }
  if (expected_msg != es.get_err_msg(0)) {
    cerr<<"Test code:\n"<<code<<"\nerrors:\n"<<es.get_err_msg(0)
        <<"\nexpected message:\n"<<expected_msg<<"\n";
    return false;
  }
  return true;
}

void semantic_fn() {
  CHECK(semantic_test("", "fn foo() {}"));
  CHECK(semantic_test(
      "function 'foo' duplicated",
      "fn foo() {}\n"
      "fn foo() {}\n"
  ));
  CHECK(semantic_test(
      "in function 'foo': argument 'x' missing type spec",
      "fn foo(x) {}"));
  CHECK(semantic_test(
      "in function 'foo': argument 'y' missing type spec",
      "fn foo(x, y) {}"));
  CHECK(semantic_test(
      "in function 'foo': argument 'z' missing type spec",
      "fn foo(x, y: Int, z) {}"));
  CHECK(semantic_test("", "fn foo(x, y: Int, z: Int) {}"));
  CHECK(semantic_test(
      "in function 'foo': unnamed argment in position 1",
      "fn foo(Int) {}"));
  CHECK(semantic_test(
      "in function 'foo': unnamed argment in position 1",
      "fn foo((Int)) {}"));
  CHECK(semantic_test(
      "in function 'foo': unnamed argment in position 1",
      "fn foo(T1 => T2) {}"));
  CHECK(semantic_test(
      "in function 'foo': unnamed argment in position 2",
      "fn foo(x: Int, T1 => T2) {}"));
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
    CHECK(parse_test(0,
        "fn foo(x) {}\n"
    ));
  }

  flags_reset();
  RETURN_TESTS_PASSED();
}
