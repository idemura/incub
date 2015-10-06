#ifndef IPED_FLAGS_HXX
#define IPED_FLAGS_HXX

#include <string>

namespace iped {
extern std::string flag_temp_path;

bool init_flags(int argc, char **argv);
}  // namespace

#endif

