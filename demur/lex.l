%option c++ full nodefault nounput noyywrap yylineno

%{
#include "base.hxx"
#include "grammar.tab.hxx"

using token = yy::parser::token;
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

"fn" { return token::FN; }
"(" { return token::LP; }
")" { return token::RP; }
"{" { return token::LC; }
"}" { return token::RC; }
"_" { return token::WILDCARD; }

  /*Floating point. Our grammar doesn't allow word to follow float. This means
    we don't need anything special to check non-letter follows floating point.
    It will be some parsing error instead. Same for integers.
  */
{D}+\.{D}+(f|d)? { return token::CONSTANT; }

  /*Integers: oct, decimal, hex*/
0o{O}+(_{O}+)*{IS}  { return token::CONSTANT; }
{D}+(_{D}+)*{IS}    { return token::CONSTANT; }
0x{H}+(_{H}+)*{IS}  { return token::CONSTANT; }

{NF}{L}* { return token::NAME; }
{TF}{L}* { return token::TYPE; }

. {
  // Treat control characters as spaces, other - error.
  if (*yytext >= 32) {
    cerr<<yylineno<<endl;
    cerr<<"Invalid character: "<<*yytext<<endl;
    yyterminate();
  }
}

%%
