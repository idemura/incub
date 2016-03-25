#ifndef IGOR_FLAGS_HXX
#define IGOR_FLAGS_HXX

#include "base.hxx"

namespace igor {

struct Flags: public FlagSet {
  bool check_names = true;
  bool log_parse = false;

  Flags() {
    register_flag("check_names", &check_names);
    register_flag("log_parse", &log_parse);
  }
};

Flags &flags();
bool flags_parse(int *argc, char **argv);
void flags_reset();

}  // namespace

#endif
