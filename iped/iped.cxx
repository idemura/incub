#include "iped.hxx"
#include "base.hxx"

namespace iped {
int main(int argc, char **argv) {
  std::cout<<"iped "<<kVersion<<std::endl;
  return 0;
}
}  // namespace

int main(int argc, char **argv) {
  return iped::main(argc, argv);
}

