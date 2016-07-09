#include "parser.hxx"

// 0 - print tests with failures
// 1 - print tests with errors
// 2 - print all
FLAG_i32(print, 0);

namespace igor {
namespace {

bool parse_test(int expected_errors, const string &code) {
  std::stringstream ss;
  ErrorLog elog("<test>", ss);
  AST ast;
  TempFile temp(code);
  if (parse(temp.get_name(), &ast, &elog)) {
    ast.analyze(&elog);
  }
  if (expected_errors != elog.count()) {
    if (flag_print >= 0) {
      cerr<<"Test code:\n"<<code
          <<"\nexpected errors: "<<expected_errors
          <<"\nactual: "<<elog.count()<<"\n";
      cerr<<ss.str();
    }
    return false;
  }
  if (flag_print >= 2 || (flag_print >= 1 && elog.count() > 0)) {
    cerr<<"Test code:\n"<<code;
    if (!code.empty() && code.back() != '\n') {
      cerr<<"\n";
    }
    cerr<<ss.str();
  }
  return true;
}

void test() {
  CHECK(parse_test(0, "fn foo(x, y: Int): Int, r: Int {}"));
}

}  // namespace
}  // namespace

int main(int argc, char **argv) {
  using namespace igor;
  std::ios_base::sync_with_stdio(false);
  if (!flags_parse(&argc, argv)) {
    return -1;
  }

  test();

  flags_reset();
  RETURN_TESTS_PASSED();
}
