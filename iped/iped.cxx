#include "iped.hxx"
#include "flags.hxx"
#include "mongoose.h"

namespace iped {
int main(int argc, char **argv) {
  if (!init_flags(&argc, argv)) {
    return -1;
  }
  cout<<"temp_path="<<flag_temp_dir()<<endl;
  cout<<"iped "<<kVersion<<endl;
  return 0;
}
}  // namespace

int main(int argc, char **argv) {
  std::ios_base::sync_with_stdio(false);
  return iped::main(argc, argv);
}

