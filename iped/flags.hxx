#ifndef IPED_FLAGS_HXX
#define IPED_FLAGS_HXX

#include "base.hxx"

namespace iped {

const string& flag_temp_dir();
const string& flag_log_file();

bool init_flags(int *argc, char **argv);

}  // namespace

#endif

