#include "demur.hxx"

namespace igor {
std::unique_ptr<Flags> flags;

int main(int argc, char **argv) {
  flags.reset(new Flags());
  if (!flags->parse(&argc, argv)) {
    return -1;
  }
  cout<<"DEMUR compiler"<<endl;
  std::vector<string> files;
  for (int i = 1; i < argc; i++) {
    files.push_back(argv[i]);
  }
  flags.reset();
  return 0;
}
}  // namespace

int main(int argc, char **argv) {
  std::ios_base::sync_with_stdio(false);
  return igor::main(argc, argv);
}
