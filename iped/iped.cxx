#include "iped.hxx"
#include "mongoose.h"

namespace iped {
std::unique_ptr<AppFlags> flags;

int main(int argc, char **argv) {
  flags.reset(new AppFlags());
  if (!flags->parse(&argc, argv)) {
    return -1;
  }
  cout<<"temp_dir="<<flags->temp_dir<<endl;
  cout<<"n32="<<flags->n32<<endl;
  cout<<"n64="<<flags->n64<<endl;
  cout<<"b="<<flags->b<<endl;
  cout<<"dbl="<<flags->dbl<<endl;
  cout<<"positional:"<<endl;
  for (int i = 0; i < argc; i++) {
    cout<<"  "<<argv[i]<<endl;
  }
  cout<<"iped "<<kVersion<<endl;
  flags.reset();
  return 0;
}
}  // namespace

int main(int argc, char **argv) {
  std::ios_base::sync_with_stdio(false);
  return iped::main(argc, argv);
}
