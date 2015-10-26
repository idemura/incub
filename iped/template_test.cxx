#include "template.hxx"

namespace iped {
namespace test {

void test1() {
  auto t = make_template("hello world\n");
  CHECK(t != nullptr);
  auto m = make_map();
  string s;
  CHECK(t.expand(m.get(), &s));
  CHECK(s == "hello world\n");
}

}  // namespace
}  // namespace

int main() {
  using namespace iped::test;
  std::ios_base::sync_with_stdio(false);
  test1();
  return 0;
}
