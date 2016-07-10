%option bison-bridge bison-locations full nodefault noyywrap reentrant yylineno

%{
#include "grammar.tab.hxx"

igor::ErrorLog *yyget_elog(yyscan_t yyscanner) {
  return reinterpret_cast<igor::ErrorLog*>(yyget_extra(yyscanner));
}

void yyset_locaton(YYLTYPE *yylloc, int line, int column) {
  yylloc->first_line = line;
  yylloc->first_column = column;
  yylloc->last_line = 0;
  yylloc->last_column = 0;
}

#define YY_USER_ACTION \
  yyset_locaton(yylloc, yylineno, yycolumn); \
  yycolumn += yyleng;

#define IGOR_LEX_ERROR \
  yyget_elog(yyscanner)->error(yylineno, yycolumn - 1).os()

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

  /*Modify & Assign*/
"+="  { return TOK_LET_PLS; }
"-="  { return TOK_LET_MNS; }

  /*Logical*/
"!"   { return TOK_NOT; }
"&&"  { return TOK_AND; }
"||"  { return TOK_OR; }
"^^"  { return TOK_XOR; }

  /*Separators*/
"=>"  { return TOK_ARROW; }
":"   { return TOK_COLON; }
","   { return TOK_COMMA; }
"."   { return TOK_DOT; }
";"   { return TOK_SEMI; }

"=="  { return TOK_CMP_EQ; }
"!="  { return TOK_CMP_NE; }
"<="  { return TOK_CMP_LE; }
">="  { return TOK_CMP_GE; }

">"   { return TOK_CMP_GT; }
"<"   { return TOK_CMP_LT; }
"="   { return TOK_LET; }

  /*Tokens with pairs*/
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
  auto c = yylval->constant = new igor::AstConstant;
  c->type = igor::AstConstant::kF64;
  c->value = string(yytext, yyleng);
  return TOK_CONSTANT;
}

  /*Integers: oct, decimal, hex*/
0o{O}+(_{O}+)*{IS}  {
  auto c = yylval->constant = new igor::AstConstant;
  c->type = igor::AstConstant::kI32;
  c->value = string(yytext, yyleng);
  return TOK_CONSTANT;
}
{D}+(_{D}+)*{IS}    {
  auto c = yylval->constant = new igor::AstConstant;
  c->type = igor::AstConstant::kI32;
  c->value = string(yytext, yyleng);
  return TOK_CONSTANT;
}
0x{H}+(_{H}+)*{IS}  {
  auto c = yylval->constant = new igor::AstConstant;
  c->type = igor::AstConstant::kI32;
  c->value = string(yytext, yyleng);
  return TOK_CONSTANT;
}

{NF}{L}* {
  yylval->name = new string(yytext, yyleng);
  return TOK_NAME;
}

_ {
  auto c = yyinput(yyscanner);
  if (c == '_' ||
      ('a' <= c && c <= 'z') ||
      ('A' <= c && c <= 'Z') ||
      ('0' <= c && c <= '9')) {
    IGOR_LEX_ERROR<<"invalid wildcard token; alpha/digit/_ should not follow _";
    yyterminate();
  }
  unput(c);
  yylval->name = new string(yytext, yyleng);
  return TOK_WILD;
}

{TF}{L}* {
  yylval->name = new string(yytext, yyleng);
  return TOK_TYPE;
}

. {
  // Treat control characters as spaces, other - error.
  if (*yytext >= 32) {
    IGOR_LEX_ERROR<<"invalid character "<<yytext;
    yyterminate();
  }
}

%%
