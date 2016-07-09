#include "parser.hxx"
#include "grammar.tab.hxx"

FLAG_bool(debug_bison, false);

namespace igor {

bool parse(const string &file_name, AST *ast) {
  auto res = false;
  auto f = fopen(file_name.c_str(), "rt");
  if (nullptr != f) {
    yyscan_t yyscanner;
    if (0 == yylex_init(&yyscanner)) {
      yyset_extra(ast, yyscanner);
      yyset_in(f, yyscanner);
#if YYDEBUG
      yydebug = flag_debug_bison;
#endif
      // Grammar accepts input till EOF.
      res = yyparse(yyscanner, ast) == 0;
      yylex_destroy(yyscanner);
    } else {
      *ast->err_sink()->format_err(0, 0)
          <<"internal error: scanner init";
      return false;
    }
    fclose(f);
  } else {
    *ast->err_sink()->format_err(0, 0)
        <<"open file error: "<<file_name;
  }
  if (!res) ast->reset();
  return res;
}

}  // namespace
