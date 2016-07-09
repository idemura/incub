#include "parser.hxx"

FLAG_bool(help, false);

namespace igor {
namespace {

bool compile(const string &file) {
  ErrorLog elog(file);
  AST ast;
  if (parse(file, &ast, &elog)) {
    return ast.analyze(&elog);
  }
  return false;
}

}  // namespace
}  // namespace

int main(int argc, char **argv) {
  using namespace igor;
  std::ios_base::sync_with_stdio(false);
  if (!flags_parse(&argc, argv)) {
    return -1;
  }

  if (argc == 1 || flag_help) {
    cout<<"The Owl Language Compiler by Igor Demura\n"
          "igor.demura@gmail.com\n"
          "\n"
          "Usage:\n"
          "  owlc [file...]\n";
    return 0;
  }

  auto err_count = 0;
  for (int i = 1; i < argc; i++) {
    err_count += compile(argv[i]);
  }
  return err_count == 0? 0: -1;
}

