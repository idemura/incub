#include "parser.hxx"
#include "grammar.tab.hxx"

FLAG_i32(debug_bison, 0);

namespace igor {

bool parse(
    const string &file_name,
    std::function<void(const string&)> error_fn) {
  auto res = false;
  auto f = fopen(file_name.c_str(), "rt");
  if (nullptr != f) {
    yyscan_t yyscanner;
    if (0 == yylex_init(&yyscanner)) {
      ScannerCtx ctx;
      ctx.error_fn = std::move(error_fn);
      yyset_extra(&ctx, yyscanner);
      yyset_in(f, yyscanner);
      yydebug = flag_debug_bison;

      // Grammar accepts input till EOF.
      res = yyparse(yyscanner) == 0;
    } else {
      error_fn("Scanner init");
    }
    fclose(f);
  } else {
    error_fn("Open file " + file_name);
  }
  return res;
}

}  // namespace
