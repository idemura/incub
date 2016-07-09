#ifndef IGOR_PARSER_HXX
#define IGOR_PARSER_HXX

#include "ast.hxx"

namespace igor {
bool parse(const string &file_name, AST *ast);
}  // namespace

#endif
