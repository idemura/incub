#include "flags.hxx"

namespace igor {
auto s_flags = std::make_unique<Flags>();

Flags& flags() {
  return *s_flags;
}

bool flags_parse(int *argc, char **argv) {
  return s_flags->parse(argc, argv);
}

void flags_reset() {
  s_flags.reset();
}
}
