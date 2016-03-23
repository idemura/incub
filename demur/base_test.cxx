#include "base.hxx"

namespace igor {
namespace {

void test_substr() {
}

class Flags: public FlagSet {
 public:
  string str = "/tmp";
  i32 n32 = 32;
  i64 n64 = 64;
  bool b = false;
  double dbl = 0.5;

  Flags() {
    register_flag("str", &str);
    register_flag("n32", &n32);
    register_flag("n64", &n64);
    register_flag("b", &b);
    register_flag("dbl", &dbl);
  }
};

void test_flags() {
  char p0[] = "test_main";
  char p1[] = "-n32";
  char p2[] = "132";
  char p3[] = "-str";
  char p4[] = "xyz";
  char* argv[] = {
    p0, p1, p2, p3, p4, nullptr
  };
  Flags flags;
  int argc = ARRAY_SIZEOF(argv) - 1;
  CHECK(flags.parse(&argc, argv));
  CHECK(argc == 1);
  CHECK(flags.n32 == 132);
  CHECK(flags.str == "xyz");
}

}  // namespace
}  // namespace

int main() {
  std::ios_base::sync_with_stdio(false);
  igor::test_substr();
  igor::test_flags();
  return TESTS_PASSED;
}

