#include "lexer.hxx"

namespace igor {
namespace {

void test_integer() {
  ErrStr err;
  auto ts = tokenize("", "0", err);
  CHECK(err.ok());
  CHECK(ts->size() == 1);
}
}  // namespace
}  // namespace

int main() {
  std::ios_base::sync_with_stdio(false);
  igor::test_integer();
  return 0;
}
