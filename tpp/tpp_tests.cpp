#include "tpp.hpp"

#include <ostream>
#include <gtest/gtest.h>

namespace idemura {
std::ostream &operator<<(std::ostream &os, char_buf const &cb) {
    return os << to_string(cb);
}

namespace tests {
TEST(CharBuf, Reset) {
    auto b1 = char_buf::strz("hello");
    EXPECT_FALSE(b1.none());
    auto b2 = std::move(b1);
    EXPECT_TRUE(b1.none());
    EXPECT_FALSE(b2.none());
    b2.reset();
    EXPECT_TRUE(b2.none());
}

TEST(CharBuf, CopyWrap) {
    auto b1 = char_buf::strz("hello");
    auto b2 = b1.copy();
    auto b3 = b1.wrap();
    std::memcpy(b1.data(), "12345", b1.size());
    EXPECT_EQ(char_buf::strz("12345"), b1);
    EXPECT_EQ(char_buf::strz("hello"), b2);
    EXPECT_EQ(char_buf::strz("12345"), b3);
}

TEST(CharBuf, Compare) {
    auto b0 = char_buf::strz("abc");
    auto b1 = char_buf::strz("abc");
    auto b2 = char_buf::strz("abc_m");
    auto b3 = char_buf::strz("abc_n");

    EXPECT_EQ(b0, b1);
    EXPECT_NE(b2, b3);

    EXPECT_LT(b1, b2);
    EXPECT_GT(b2, b1);

    EXPECT_LE(b1, b0);
    EXPECT_LE(b0, b1);

    EXPECT_LT(b2, b3);
    EXPECT_LE(b2, b3);

    EXPECT_GT(b3, b2);
    EXPECT_GE(b3, b2);
}

TEST(CharBuf, SubStr) {
    auto b1 = char_buf::strz("kit cat");
    auto b2 = b1.wrap();
    b2.move(4);
    EXPECT_EQ(char_buf::strz("cat"), b2);
    EXPECT_EQ(char_buf::strz("t c"), b1.substr(2, 3));
}

TEST(StringTable, Test) {
    details::string_table st;
    auto id1 = st.insert(char_buf::strz("hello"));
    auto id2 = st.insert(char_buf::strz("world"));
    EXPECT_EQ(char_buf::strz("hello"), st.string(id1));
    EXPECT_EQ(char_buf::strz("world"), st.string(id2));
}

TEST(TokenCursor, Test) {
    using details::token;

    auto code = char_buf::strz(
            "let x 10\n"
            "let y \"hello\"\n"
    );
    details::token_cursor tk{code.wrap()};
    EXPECT_TRUE(tk.valid());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::kw_let);
    EXPECT_TRUE(tk.text() == char_buf::strz("let"));

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::symbol);
    EXPECT_EQ(char_buf::strz("x"), tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::literal_int);
    EXPECT_EQ(char_buf::strz("10"), tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::line_end);
    EXPECT_EQ(char_buf::strz("\n"), tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::kw_let);
    EXPECT_TRUE(tk.text() == char_buf::strz("let"));

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::symbol);
    EXPECT_EQ(char_buf::strz("y"), tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::literal_str);
    EXPECT_EQ(char_buf::strz("hello"), tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::line_end);
    EXPECT_EQ(char_buf::strz("\n"), tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::end);
    EXPECT_TRUE(tk.valid());
    // When end is reached, we stay in this state.
    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::end);
    EXPECT_TRUE(tk.valid());
}

TEST(TPP, Compile) {
    auto program = compile_template(char_buf::strz(
            "let x 10\n"
            "let y \"hello\"\n"
    ));
    ASSERT_TRUE(program);
    string_stream ss;
    program->run(&ss);
    auto s = ss.release();
    std::cout<<s<<std::endl;
}
}
}
