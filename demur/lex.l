%option c++
%option noyywrap

%{
#include <iostream>
%}

L       [a-zA-Z0-9_]
D       [0-9]
H       [0-9a-fA-F]
NF      [a-z]
TF      [A-Z]

%%

[ \t\r\f\n] ;
{D}+\.{D}+ {
  //std::cout << "floating-point: " << yytext << std::endl;
}
{D}+(_{D}+)* {
  std::cout << "integer: " << yytext << std::endl;
}
{NF}{L}* {
  //std::cout << "name: " << yytext << std::endl;
  return 12;
}
{TF}{L}* {
  //std::cout << "type_name: " << yytext << std::endl;
  return 11;
}
_ {
  //std::cout<< "wildcard:"<<yytext<<std::endl;
}

%%
