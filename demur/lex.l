%option c++ full nodefault nounput noyywrap yylineno

%{
#include <string>
#include "grammar.tab.hxx"

#if 1  // For debug puposes.
#define IGOR_IO 1
#include <iostream>
using std::cerr;
using std::cout;
using std::endl;
#else
#define IGOR_IO 0
#endif

using std::string;
%}

S       [ \t\r\f]
L       [a-zA-Z0-9_]
O       [0-7]
D       [0-9]
H       [0-9a-fA-F]
NF      [a-z]
TF      [A-Z]

%%
  /*Spaces, comment till end of line or EOF*/
{S}|\n|#.*

  /*Floating point*/
{D}+\.{D}+(f|d)?/[^a-zA-Z0-9_] {
  //cout<<"floating-point: "<<yytext<<endl;
}
  /*Integers: oct, decimal, hex*/
0o{O}+(_{O}+)* {
  cout<<"oct integer: "<<yytext<<endl;
}
{D}+(_{D}+)* {
  cout<<"dec integer: "<<yytext<<endl;
}
0x{H}+(_{H}+)* {
  cout<<"hex integer: "<<yytext<<endl;
}

{NF}{L}* {
  //cout<<"name: "<<yytext<<endl;
  return 12;
}
{TF}{L}* {
  //cout<<"type_name: "<<yytext<<endl;
  // cout<<"line #"<<yylineno<<endl;
  return 11;
}
_ {
  //cout<<"wildcard: "<<yytext<<endl;
}
. {
  // Treat control characters as spaces, other - error.
  if (*yytext >= 32) {
    cerr<<yylineno<<endl;
    cerr<<"Invalid character: "<<*yytext<<endl;
    yyterminate();
  }
}

%%
