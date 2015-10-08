#include "flags.hxx"

namespace iped {
namespace test {

class Flags: public FlagSet {
 public:
  string str = "/tmp";
  i32 n32 = 32;
  i64 n64 = 64;
  bool b = false;
  double dbl = 0.5;

  Flags() {
    register_flag("str", &);
    register_flag("n32", &n32);
    register_flag("n64", &n64);
    register_flag("b", &b);
    register_flag("dbl", &dbl);
  }
};

void test1() {

}
  
}  // namespace
}  // namespace

int main() {
  using namespace iped::test;
  test1();
  return 0;
}
