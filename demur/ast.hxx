#ifndef IGOR_AST_HXX
#define IGOR_AST_HXX

#include "lexer.hxx"

namespace igor {

void build_ast(TokenStream *tokens, ErrStr &err);

}

#endif
