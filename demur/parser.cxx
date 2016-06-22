#include "parser.hxx"
#include "grammar.tab.hxx"
#include <FlexLexer.h>

FLAG_i32(bison_debug, 0);

namespace igor {

bool parse(std::istream *in, std::function<void(const string&)> error_fn) {
  yyFlexLexer lexer(in);
  yy::parser parser(&lexer, std::move(error_fn));
  if (flag_bison_debug > 0) {
    parser.set_debug_level(flag_bison_debug);
  }
  int r = 0;
  while (!in->eof() && r == 0) {
    r = parser.parse();
  }
  return r == 0;
}

}  // namespace
