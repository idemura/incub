#include "base.hxx"

FLAG_string(str, "/tmp");
FLAG_i32(n32, 32);
FLAG_i64(n64, 64);
FLAG_bool(b, false);
FLAG_double(dbl, 0.5);

namespace igor {
namespace {

void test_flags() {
  char p0[] = "test_main";
  char p1[] = "-n32";
  char p2[] = "132";
  char p3[] = "-str";
  char p4[] = "xyz";
  char p5[] = "-b+";
  char* argv[] = {
    p0, p1, p2, p3, p4, p5, nullptr
  };
  int argc = ARRAY_SIZEOF(argv) - 1;
  CHECK(flags_parse(&argc, argv));
  CHECK(argc == 1);
  CHECK(flag_n32 == 132);
  CHECK(flag_str == "xyz");
  CHECK(flag_b);
  flags_reset();
  CHECK(flag_str.empty());
}

}  // namespace
}  // namespace

int main(int argc, char **argv) {
  using namespace igor;

  test_flags();

  RETURN_TESTS_PASSED();
}
