#include "demur.hxx"

#include <fstream>
#include "lexer.hxx"

namespace igor {
std::unique_ptr<Flags> flags;

bool compile(const string &file_name, std::istream &is) {
  std::stringstream buffer;
  buffer << is.rdbuf();
  ErrStr err;
  auto tokens = tokenize(file_name, buffer.str(), err);
  if (!err.ok()) {
    cerr<<err.to_string();
  }
  return err.ok();
}

int main(int argc, char **argv) {
  flags.reset(new Flags());
  if (!flags->parse(&argc, argv)) {
    return -1;
  }
  auto failed = false;
  if (argc == 1) {
    failed = compile("<stdin>", std::cin);
  } else {
    for (int i = 1; i < argc; i++) {
      auto file_name = string(argv[i]);
      std::ifstream fs(file_name);
      if (!fs.good()) {
        cerr<<"Failed to open "<<file_name<<endl;
        failed = true;
      } else if (!compile(file_name, fs)) {
        failed = true;
      }
    }
  }
  flags.reset();
  return failed ? -1 : 0;
}
}  // namespace

int main(int argc, char **argv) {
  std::ios_base::sync_with_stdio(false);
  return igor::main(argc, argv);
}
