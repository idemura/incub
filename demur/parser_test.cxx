#include "parser.hxx"

namespace igor {
namespace {

std::function<void(const string&)> error_h(int *counter) {
  return [counter](const string &msg) {
    cerr<<"Parser error: "<<msg<<endl;
    *counter += 1;
  };
}

void test_comment() {
  TempFile temp(
      "# comment till eol\n"
      "# comment"
  );
  int ec = 0;
  CHECK(parse(temp.get_name(), error_h(&ec)));
  CHECK(ec == 0);
}

void test_fn() {
  TempFile temp(
      "fn foo() {}\n"
      "fn foo(n: Int) {}\n"
      "fn bar(x, y, z: Int) {}\n"
      "fn bar(x, y: Int, s: String) {}\n"
  );
  int ec = 0;
  CHECK(parse(temp.get_name(), error_h(&ec)));
  CHECK(ec == 0);
}

void test_fn_na() {
  TempFile temp(
      "fn foo(n: Int,) {}\n"
  );
  int ec = 0;
  CHECK(!parse(temp.get_name(), error_h(&ec)));
  CHECK(ec == 1);
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
  test_fn_na();

  flags_reset();
  RETURN_TESTS_PASSED();
}
