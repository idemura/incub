#include "base.hxx"

#include <FlexLexer.h>

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

  yyFlexLexer lexer;
  lexer.switch_streams(&ss);
  while (lexer.yylex()) {
    // Empty
  }
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

  flags_reset();
  RETURN_TESTS_PASSED();
}
