#include "parser.hxx"

namespace igor {
namespace {

int parse_test(string s) {
  TempFile temp(s);
  int err_count = 0;
  parse(temp.get_name(),
      [&err_count](const string &msg) {
        // cerr<<"Parser error: "<<msg<<endl;
        err_count += 1;
      });
  return err_count;
}

void test_comment() {
  CHECK(0 == parse_test(
      "# comment till eol\n"
      "# comment"
  ));
}

void test_fn() {
  CHECK(0 == parse_test(
      "fn foo() {}\n"
      "fn foo(n: Int) {}\n"
      "fn bar(x, y, z: Int) {}\n"
      "fn bar(x, y: Int, s: String) {}\n"
      "fn foo(): Int {}\n"
      "fn foo(x: Int): Int {}\n"
      "fn foo(x: Int): x: Int {}\n"
      "fn foo(x: Int): x, y: Int {}\n"
      "fn foo(x: Int): x: Int, y: Int {}\n"
      "fn foo(x: Int): Int, Int {}\n"
  ));
  // This kind of arg list is accepted by grammar, but possibly should be
  // rejected by semantic analyzer.
  CHECK(0 == parse_test(
      "fn foo(Int) {}\n"
      "fn foo(Int, Int) {}\n"
  ));
  CHECK(1 == parse_test(
      "fn foo(: Int) {}\n"
  ));
  CHECK(1 == parse_test(
      "fn foo(n: Int,) {}\n"
  ));
  CHECK(1 == parse_test(
      "fn foo(n,: Int) {}\n"
  ));
  CHECK(1 == parse_test(
      "fn foo(n: int) {}\n"
  ));
  CHECK(1 == parse_test(
      "fn foo(n:) {}\n"
  ));
  CHECK(1 == parse_test(
      "fn foo(n: Int): {}\n"
  ));
}

void test_type_spec() {
  // Use that function grammar accepts just type_spec.
  CHECK(0 == parse_test(
      "fn foo(T) {}\n"
      "fn foo((T)) {}\n"
      "fn foo([T, T]) {}\n"
      "fn foo(([T, T])) {}\n"
      "fn foo(T => T) {}\n"
      "fn foo(T[]) {}\n"
      "fn foo(T => T[]) {}\n"
      "fn foo(T[] => T) {}\n"
      "fn foo([T, T] => T[]) {}\n"
      "fn foo(T => [T, T] => T[]) {}\n"
      "fn foo(T => (T => T)[] => (T => [T, T])) {}\n"
      "# big sample:\n"
      "fn foo(f: T => (T => T)[] => (T => [T, T]), n: T):\n"
      "    [T, T, T]\n {"
      "}\n"
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

  flags_reset();
  RETURN_TESTS_PASSED();
}
