#include "ast.hxx"

namespace igor {
namespace {

void test() {
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
