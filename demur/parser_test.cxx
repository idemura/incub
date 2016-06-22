#include "parser.hxx"

namespace igor {
namespace {

void test1() {
  std::stringstream ss;
  ss<<"10 1_0 0o1_7 0o1_7 0x19af 0x19_af\n"
      "0.5 0.2f 0.6d\n"
      "TypeName T Underscore_Type_Name\n"
      "igor hello x my_id\n"
      "# comment till eol\n"
      "# comment";

  CHECK(parse(&ss));
}

void test2() {
  // std::stringstream ss;
  // CHECK(parse(&ss));
}

}  // namespace
}  // namespace

int main(int argc, char **argv) {
  using namespace igor;
  std::ios_base::sync_with_stdio(false);
  if (!flags_parse(&argc, argv)) {
    return -1;
  }

  test1();
  test2();

  flags_reset();
  RETURN_TESTS_PASSED();
}
