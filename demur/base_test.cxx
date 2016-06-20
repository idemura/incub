#include "base.hxx"

namespace igor {
namespace {

FLAG_string(str, "/tmp");
FLAG_i32(n32, 32);
FLAG_i64(n64, 64);
FLAG_bool(b, false);
FLAG_double(dbl, 0.5);

void test_flags() {
  char p0[] = "test_main";
  char p1[] = "-n32";
  char p2[] = "132";
  char p3[] = "-str";
  char p4[] = "xyz";
  char* argv[] = {
    p0, p1, p2, p3, p4, nullptr
  };
  int argc = ARRAY_SIZEOF(argv) - 1;
  CHECK(flags_parse(&argc, argv));
  CHECK(argc == 1);
  CHECK(flag_n32 == 132);
  CHECK(flag_str == "xyz");
  CHECK(flag_dbl == 0.5);
  flags_reset();
}

}  // namespace
}  // namespace

int main(int argc, char **argv) {
  using namespace igor;

  test_flags();

  return TESTS_PASSED();
}
