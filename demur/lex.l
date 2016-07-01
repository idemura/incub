%option bison-bridge full nodefault nounput noyywrap reentrant yylineno

%{
#include "grammar.tab.hxx"

igor::AST *yyget_ast(yyscan_t yyscanner) {
  return reinterpret_cast<igor::AST*>(yyget_extra(yyscanner));
}
%}

S       [ \t\r\f]
L       [a-zA-Z0-9_]
O       [0-7]
D       [0-9]
H       [0-9a-fA-F]
NF      [a-z]
TF      [A-Z]
IS      (i8|i16|i32|i64)?

%%

  /*Spaces, comment till end of line or EOF*/
{S}|\n|#.*

"fn" { return TOK_FN; }

"=>"  { return TOK_ARROW; }
":"   { return TOK_COLON; }
","   { return TOK_COMMA; }
"."   { return TOK_DOT; }
"["   { return TOK_LB; }
"]"   { return TOK_RB; }
"("   { return TOK_LP; }
")"   { return TOK_RP; }
"{"   { return TOK_LC; }
"}"   { return TOK_RC; }
"_"   { return TOK_WILDCARD; }

  /*Floating point. Our grammar doesn't allow word to follow float. This means
    we don't need anything special to check non-letter follows floating point.
    It will be some parsing error instead. Same for integers.
  */
{D}+\.{D}+(f|d)? { return TOK_CONSTANT; }

  /*Integers: oct, decimal, hex*/
0o{O}+(_{O}+)*{IS}  { return TOK_CONSTANT; }
{D}+(_{D}+)*{IS}    { return TOK_CONSTANT; }
0x{H}+(_{H}+)*{IS}  { return TOK_CONSTANT; }

{NF}{L}* {
  yylval->name = new string(yytext, yyleng);
  return TOK_NAME;
}

{TF}{L}* {
  yylval->name = new string(yytext, yyleng);
  return TOK_TYPE;
}

. {
  // Treat control characters as spaces, other - error.
  if (*yytext >= 32) {
    // We don't have good reporting capability in lexer. Let parser do this in
    // @yylex.
    return TOK_INVALID_CHAR;
  }
}

%%
