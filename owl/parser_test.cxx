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

void test_comment() {
  CHECK(parse_test(0,
      "# comment till eol\n"
      "# comment"
  ));
}

void test_fn_spec() {
  CHECK(parse_test(0, "fn foo() {}"));
  CHECK(parse_test(0, "fn foo(n: Int) {}"));
  CHECK(parse_test(0, "fn foo(x, y, z: Int) {}"));
  CHECK(parse_test(0, "fn foo(x, y: Int, s: String) {}"));
  CHECK(parse_test(0, "fn foo(): Int {}"));
  CHECK(parse_test(0, "fn foo(x: Int): Int {}"));
  CHECK(parse_test(0, "fn foo(x: Int): x: Int {}"));
  CHECK(parse_test(0, "fn foo(x: Int): x, y: Int {}"));
  CHECK(parse_test(0, "fn foo(x: Int): x: Int, y: Int {}"));
  CHECK(parse_test(0, "fn foo(x: Int): Int, Int {}"));
  CHECK(parse_test(0, "fn foo(_: Int) {}"));
  CHECK(parse_test(0, "fn foo(_, y: Int) {}"));
  CHECK(parse_test(0, "fn foo(y, _: Int) {}"));
  CHECK(parse_test(2, "fn foo(y, _1: Int) {}"));
  CHECK(parse_test(2, "fn foo(y, _a: Int) {}"));

  CHECK(parse_test(1, "fn foo(: Int) {}\n"));
  CHECK(parse_test(1, "fn foo(n: Int,) {}\n"));
  CHECK(parse_test(1, "fn foo(n,: Int) {}\n"));
  CHECK(parse_test(1, "fn foo(n: int) {}\n"));
  CHECK(parse_test(1, "fn foo(n:) {}\n"));
  CHECK(parse_test(1, "fn foo(n: Int): {}\n"));

  CHECK(parse_test(1,
      "fn foo() {}\n"
      "fn foo() {}\n"
  ));
  CHECK(parse_test(1, "fn foo(x) {}"));
  CHECK(parse_test(1, "fn foo(x, y) {}"));
  CHECK(parse_test(1, "fn foo(x, y: Int, z) {}"));
  CHECK(parse_test(0, "fn foo(x, y: Int, z: Int) {}"));
  CHECK(parse_test(1, "fn foo(Int) {}"));
  CHECK(parse_test(1, "fn foo((Int)) {}"));
  CHECK(parse_test(1, "fn foo(T1 => T2) {}"));
  CHECK(parse_test(1, "fn foo(x: Int, T1 => T2) {}"));
}

void test_type() {
  // Use that function grammar accepts just type_spec.
  CHECK(parse_test(0, "fn foo(x: T) {}"));
  CHECK(parse_test(0, "fn foo(x: (T)) {}"));
  CHECK(parse_test(0, "fn foo(x: [T]) {}"));
  CHECK(parse_test(0, "fn foo(x: [T1, T2]) {}"));
  CHECK(parse_test(0, "fn foo(x: ([T1, T2])) {}"));
  CHECK(parse_test(0, "fn foo(x: T1 => T2) {}"));
  CHECK(parse_test(0, "fn foo(x: T[]) {}"));
  CHECK(parse_test(0, "fn foo(x: T1 => T2[]) {}"));
  CHECK(parse_test(0, "fn foo(x: T1[] => T2) {}"));
  CHECK(parse_test(0, "fn foo(x: [T1, T2] => T[]) {}"));
  CHECK(parse_test(0, "fn foo(x: T => [T, T] => T[]) {}"));
  CHECK(parse_test(0, "fn foo(x: T1 => (T2 => T2)[] => (T3 => [T, T])) {}"));
  CHECK(parse_test(0, "fn foo(x: module.T) {}"));
  CHECK(parse_test(0, "fn foo(x: module.sub.T) {}"));
  CHECK(parse_test(0,
      "# big sample:\n"
      "fn foo(f: T => (T => T)[] => (T => [T, T]), n: T):\n"
      "    [T, T, T]\n {"
      "}\n"
  ));
}

void test_expr() {
  CHECK(parse_test(0,
      "fn foo(x: T) {\n"
      "  return;\n"
      "  x;\n"
      "  return x;\n"
      "  x = y;\n"
      "  return x, x = y;\n"
      "}\n"
  ));
}

}  // namespace
}  // namespace

int main(int argc, char **argv) {
  using namespace igor;
  std::ios_base::sync_with_stdio(false);
  if (!flags_parse(&argc, argv)) {
    return -1;
  }

  test_comment();
  test_fn_spec();
  test_type();
  test_expr();

  if (1) {
    CHECK(parse_test(0,
        "fn foo() {\n"
        "  return;\n"
        "}\n"
    ));
  }

  flags_reset();
  RETURN_TESTS_PASSED();
}
