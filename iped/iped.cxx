#include "iped.hxx"
#include "base.hxx"

namespace iped {
int main(int argc, char **argv) {
  std::cout<<"iped "<<kVersion<<std::endl;
  return 0;
}
}  // namespace

int gn = 0;
static struct InitFlags {
  InitFlags() {
    gn = 10;
  }
} init_flags;

int main(int argc, char **argv) {
  std::ios_base::sync_with_stdio(false);
  std::cout<<gn<<std::endl;
  return iped::main(argc, argv);
}

