#include "base.hxx"

#include <FlexLexer.h>

namespace igor {
namespace {

void test1() {
  std::stringstream ss;
  ss<<"1 10_0 0.5\n"
      "TypeName T Underscore_Type_Name\n"
      "igor hello x my_id\n";

  cout<<"version 1"<<endl;
  yyFlexLexer lexer;
  lexer.switch_streams(&ss);
  int r = 0;
  while ((r = lexer.yylex()) != 0) {
    cout<<"r="<<r<<endl;
  }
  // // Lex through the input:
  // auto r = lexer.yylex();
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
  return TESTS_PASSED();
}
