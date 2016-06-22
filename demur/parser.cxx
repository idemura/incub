#include "parser.hxx"
#include "grammar.tab.hxx"
#include <FlexLexer.h>

namespace igor {

bool parse(std::stringstream *ss) {
  yyFlexLexer lexer(ss);
  yy::parser parser(&lexer);
  int r = 0;
  while (!ss->eof() && r == 0) {
    r = parser.parse();
  }
  return r == 0;
}

}  // namespace
