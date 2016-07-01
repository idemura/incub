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
  CHECK(0 == parse_test(
      "fn f10() {}\n"
      "fn f11(n: Int) {}\n"
      "fn f12(x, y, z: Int) {}\n"
      "fn f13(x, y: Int, s: String) {}\n"
      "fn f14(): Int {}\n"
      "fn f15(x: Int): Int {}\n"
      "fn f16(x: Int): x: Int {}\n"
      "fn f17(x: Int): x, y: Int {}\n"
      "fn f18(x: Int): x: Int, y: Int {}\n"
      "fn f19(x: Int): Int, Int {}\n"
  ));
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
}

void test_type_spec() {
  // Use that function grammar accepts just type_spec.
  CHECK(0 == parse_test(
      "fn f10(T) {}\n"
      "fn f10(module.T) {}\n"
      // "fn f10(module.sub.T) {}\n"
      "fn f11((T)) {}\n"
      "fn f12([T, T]) {}\n"
      "fn f13(([T, T])) {}\n"
      "fn f14(T => T) {}\n"
      "fn f15(T[]) {}\n"
      "fn f16(T => T[]) {}\n"
      "fn f17(T[] => T) {}\n"
      "fn f18([T, T] => T[]) {}\n"
      "fn f19(T => [T, T] => T[]) {}\n"
      "fn f20(T => (T => T)[] => (T => [T, T])) {}\n"
      "# big sample:\n"
      "fn f21(f: T => (T => T)[] => (T => [T, T]), n: T):\n"
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
  // test();

  flags_reset();
  RETURN_TESTS_PASSED();
}
