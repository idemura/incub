%option bison-bridge bison-locations full nodefault noyywrap reentrant yylineno

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

"elif" { return TOK_ELIF; }
"else" { return TOK_ELSE; }
"fn" { return TOK_FN; }
"if" { return TOK_IF; }
"return" { return TOK_RETURN; }

"=>"  { return TOK_ARROW; }
":"   { return TOK_COLON; }
","   { return TOK_COMMA; }
"."   { return TOK_DOT; }
"="   { return TOK_LET; }
"=+"  { return TOK_LET_PLS; }
"=-"  { return TOK_LET_MNS; }
";"   { return TOK_SEMI; }
"["   { return TOK_LB; }
"]"   { return TOK_RB; }
"("   { return TOK_LP; }
")"   { return TOK_RP; }
"{"   { return TOK_LC; }
"}"   { return TOK_RC; }

  /*Floating point. Our grammar doesn't allow word to follow float. This means
    we don't need anything special to check non-letter follows floating point.
    It will be some parsing error instead. Same for integers.
  */
{D}+\.{D}+(f|d)? {
  yylval->constant = new igor::AstConstant;
  yylval->constant->type = igor::AstConstant::kF64;
  yylval->constant->value = string(yytext, yyleng);
  return TOK_CONSTANT;
}

  /*Integers: oct, decimal, hex*/
0o{O}+(_{O}+)*{IS}  {
  yylval->constant = new igor::AstConstant;
  yylval->constant->type = igor::AstConstant::kI32;
  yylval->constant->value = string(yytext, yyleng);
  return TOK_CONSTANT;
}
{D}+(_{D}+)*{IS}    {
  yylval->constant = new igor::AstConstant;
  yylval->constant->type = igor::AstConstant::kI32;
  yylval->constant->value = string(yytext, yyleng);
  return TOK_CONSTANT;
}
0x{H}+(_{H}+)*{IS}  {
  yylval->constant = new igor::AstConstant;
  yylval->constant->type = igor::AstConstant::kI32;
  yylval->constant->value = string(yytext, yyleng);
  return TOK_CONSTANT;
}

{NF}{L}* {
  yylval->name = new string(yytext, yyleng);
  return TOK_NAME;
}

_ {
  yylval->name = new string(yytext, yyleng);
  auto c = yyinput(yyscanner);
  if (c == '_' ||
      ('a' <= c && c <= 'z') ||
      ('A' <= c && c <= 'Z') ||
      ('0' <= c && c <= '9')) {
    yyterminate();
  }
  unput(c);
  return TOK_WILD;
}

{TF}{L}* {
  yylval->name = new string(yytext, yyleng);
  return TOK_TYPE;
}

. {
  // Treat control characters as spaces, other - error.
  if (*yytext >= 32) {
    yyterminate();
  }
}

%%
