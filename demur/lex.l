%option noyywrap
%{
#include <iostream>
using namespace std;

namespace {
int line_num = 1;
}  // namespace
%}

L       [a-zA-Z0-9_]
D       [0-9]
H       [0-9a-fA-F]
NF      [a-z]
TF      [A-Z]

%%

[ \t\r\f\n] ;
{D}+\.{D}+ {
  cout << "floating-point: " << yytext << endl;
}
{D}+ {
  cout << "integer: " << yytext << endl;
}
{NF}{L}* {
  cout << "name: " << yytext << endl;
  return 100;
}
{TF}{L}* {
  cout << "type_name: " << yytext << endl;
}
_ {
  cout<< "wildcard:"<<yytext<<endl;
}

%%

int main(int argc, char** argv) {
  cout<<"version 1"<<endl;
  yyFlexLexer lex;
  // Lex through the input:
  auto r = lex.yylex();
  cout<<"r="<<r<<endl;
  cout<<"once more:\n";
  cout<<lex.yylex()<<endl;
}
