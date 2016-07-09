#include "parser.hxx"
#include "grammar.tab.hxx"

FLAG_bool(debug_bison, false);

namespace igor {

bool parse(const string &file_name, AST *ast, ErrorLog *elog) {
  auto res = false;
  auto f = fopen(file_name.c_str(), "rt");
  if (nullptr != f) {
    yyscan_t yyscanner;
    if (0 == yylex_init(&yyscanner)) {
      yyset_extra(elog, yyscanner);
      yyset_in(f, yyscanner);
#if YYDEBUG
      yydebug = flag_debug_bison;
#endif
      // Grammar accepts input till EOF.
      res = yyparse(yyscanner, ast, elog) == 0;
      yylex_destroy(yyscanner);
    } else {
      elog->error(0, 0).os()<<"internal error: scanner init";
      return false;
    }
    fclose(f);
  } else {
    elog->error(0, 0).os()<<"open file error: "<<file_name;
  }
  return res;
}

}  // namespace
