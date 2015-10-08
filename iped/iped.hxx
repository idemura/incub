#ifndef IPED_IPED_HXX
#define IPED_IPED_HXX

#include "flags.hxx"

namespace iped {
constexpr int kVersion = 1;

class AppFlags: public Flags {
 public:
  string temp_dir = "/tmp";
  i32 n32 = 32;
  i64 n64 = 64;
  bool b = false;
  double dbl = 0.5;

  AppFlags() {
    register_flag("temp_dir", &temp_dir);
    register_flag("n32", &n32);
    register_flag("n64", &n64);
    register_flag("b", &b);
    register_flag("dbl", &dbl);
  }
};

extern std::unique_ptr<AppFlags> flags;

}  // namespace

#endif

