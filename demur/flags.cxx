#include "flags.hxx"

namespace igor {
auto s_flags = std::make_unique<Flags>();

Flags& flags() {
  return *s_flags;
}

bool flags_parse(int *argc, char **argv) {
  if (!s_flags->parse(argc, argv)) {
    flags_reset();
    return false;
  }
  return true;
}

void flags_reset() {
  s_flags.reset();
}
}
