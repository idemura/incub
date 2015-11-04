#include "template.hxx"

namespace iped {
namespace {

void test_basic() {
  auto t = make_template("hello world\n");
  CHECK(t != nullptr);
  auto m = make_dict();
  string s;
  CHECK(t->expand(m.get(), &s));
  CHECK(s == "hello world\n");
}

}  // namespace
}  // namespace

int main() {
  std::ios_base::sync_with_stdio(false);
  iped::test_basic();
  return 0;
}
