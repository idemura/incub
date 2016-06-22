#include "parser.hxx"

namespace igor {
namespace {

std::function<void(const string&)> error_h(int *counter) {
  return [counter](const string &msg) {
    cerr<<"Parser error: "<<msg<<endl;
    *counter += 1;
  };
}

void test1() {
  std::stringstream ss;
  ss<<"10 1_0 0o1_7 0o1_7 0x19af 0x19_af\n"
      "0.5 0.2f 0.6d\n"
      "TypeName T Underscore_Type_Name\n"
      "igor hello x my_id\n"
      "# comment till eol\n"
      "# comment";

  int ec = 0;
  CHECK(parse(&ss, error_h(&ec)));
  CHECK(ec == 0);
}

void test2() {
  std::stringstream ss;
  ss<<"# first sample\n"
      "fn foo(n) {}\n";
  int ec = 0;
  CHECK(parse(&ss, error_h(&ec)));
  CHECK(ec == 0);
}

}  // namespace
}  // namespace

int main(int argc, char **argv) {
  using namespace igor;
  std::ios_base::sync_with_stdio(false);
  if (!flags_parse(&argc, argv)) {
    return -1;
  }

  // test1();
  test2();

  flags_reset();
  RETURN_TESTS_PASSED();
}
