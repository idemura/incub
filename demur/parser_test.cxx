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
  ));
  CHECK(1 == parse_test(
      "fn foo(n: Int,) {}\n"
  ));
  CHECK(1 == parse_test(
      "fn foo(n,: Int) {}\n"
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

  flags_reset();
  RETURN_TESTS_PASSED();
}
