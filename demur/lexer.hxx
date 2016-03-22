#ifndef IGOR_LEXER_HXX
#define IGOR_LEXER_HXX

#include "base.hxx"

namespace igor {

enum class TokType : i64 {
  Literal,
  Function,
  Colon,
};

enum class LitType : i64 {
  Int,
  Int8,
  Int16,
  Int32,
  Int64,
  Float32,
  Float64,
  Char,
  String,
};

struct Token {
  int line = 0;
  int col = 0;
  TokType type;
};

struct TokLiteral {
  LitType type;
};

// For Char as well.
struct TokInteger : public TokLiteral {
  i64 n = 0;
};

struct TokFloatPt : public TokLiteral {
  double f = 0;
};

struct TokString : public TokLiteral {
  string s;
};

class TokenStream {
public:
  TokenStream() = default;
  void add(Token t);

private:
  std::string file_name;
  std::vector<Token> tokens;
};

bool tokenize(
    const std::string &file_name,
    std::string s,
    TokenStream *tokens,
    ErrStr &es);

}

#endif
