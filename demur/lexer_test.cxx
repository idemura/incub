#include "lexer.hxx"

namespace igor {
namespace {

void print_tokens(const TokenStream &ts) {
  for (auto c = ts.cursor(); !c.done(); c.next()) {
    cout<<*c.at()<<endl;
  }
  cout<<"TokenStream: "<<ts.size()<<" tokens"<<endl;
}

void test_case_fail(string s) {
  ErrStr err;
  auto ts = tokenize("<test>", std::move(s), err);
  CHECK(!err.ok());
  // cerr<<err.str()<<endl;
}

void test_case_integer(string s, i64 expected) {
  ErrStr err;
  auto ts = tokenize("<test>", std::move(s), err);
  // print_tokens(*ts);
  CHECK(err.ok());
  CHECK(ts->size() == 2);
  auto c = ts->cursor();
  CHECK(!c.done());
  CHECK(c.at()->type == TokType::Integer);
  const auto payload = get_payload<Literal<i64>>(*c.at());
  CHECK(payload.type == LitType::Int);
  CHECK(payload.val == expected);
  c.next();
  CHECK(!c.done());
  CHECK(c.at()->type == TokType::EndFile);
  c.next();
  CHECK(c.done());
}

void test_integer() {
  test_case_integer("0", 0);
  test_case_integer("00", 0);
  test_case_integer("08", 8);
  test_case_integer("0_9", 9);
  test_case_fail("0_");
  test_case_fail("0__1");
  test_case_integer("2", 2);
  test_case_integer("10", 10);
  test_case_integer("12", 12);
  test_case_integer("1_2", 12);
  test_case_integer("1_23", 123);
  test_case_integer("12_3", 123);
  test_case_integer("1_2_3", 123);
}

void test_case_name(string s, const string &expected) {
  ErrStr err;
  auto ts = tokenize("<test>", std::move(s), err);
  // print_tokens(*ts);
  CHECK(err.ok());
  CHECK(ts->size() == 2);
  auto c = ts->cursor();
  CHECK(!c.done());
  CHECK(c.at()->type == TokType::Name);
  const auto payload = get_payload<string>(*c.at());
  CHECK(payload == expected);
  c.next();
  CHECK(!c.done());
  CHECK(c.at()->type == TokType::EndFile);
  c.next();
  CHECK(c.done());
}

void test_name() {
  test_case_name("alpha", "alpha");
  test_case_name("functionz", "functionz");
}

void test_case_simple(string s, TokType type) {
  ErrStr err;
  auto ts = tokenize("<test>", std::move(s), err);
  // print_tokens(*ts);
  CHECK(err.ok());
  CHECK(ts->size() == 2);
  auto c = ts->cursor();
  CHECK(!c.done());
  CHECK(c.at()->type == type);
  c.next();
  CHECK(!c.done());
  CHECK(c.at()->type == TokType::EndFile);
  c.next();
  CHECK(c.done());
}

void test_keyword() {
  test_case_simple("function", TokType::Function);
  test_case_simple("(", TokType::LParen);
  test_case_simple(")", TokType::RParen);
  test_case_simple("{", TokType::LCurly);
  test_case_simple("}", TokType::RCurly);
  test_case_simple("[", TokType::LBracket);
  test_case_simple("]", TokType::RBracket);
}

}  // namespace
}  // namespace

int main() {
  std::ios_base::sync_with_stdio(false);
  igor::test_integer();
  igor::test_name();
  igor::test_keyword();
  return TESTS_PASSED;
}
