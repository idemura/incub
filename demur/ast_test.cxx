#include "ast.hxx"
#include "flags.hxx"

namespace igor {
namespace {

void test1() {
  ErrStr err;
  auto ts = tokenize("<test>", "function foo()\n{\n}\n", err);
  CHECK(err.ok());
  build_ast(ts.get(), err);
  cout<<err.str();
  CHECK(err.ok());
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
  return TESTS_PASSED;
}
