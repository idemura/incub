#include "tpp.hpp"

#include <ostream>
#include <gtest/gtest.h>

namespace idemura {
std::ostream &operator<<(std::ostream &os, str_view sv) {
    return os << to_string(sv);
}

namespace tests {
TEST(CharBuf, Reset) {
    auto b1 = char_buf::strz("hello");
    EXPECT_TRUE(b1.data() != nullptr);
    auto b2 = std::move(b1);
    EXPECT_TRUE(b1.data() == nullptr);
    EXPECT_TRUE(b2.data() != nullptr);
    b2.reset();
    EXPECT_TRUE(b2.data() == nullptr);
}

TEST(CharBuf, CopyWrap) {
    auto b1 = char_buf::strz("hello");
    auto b2 = b1.copy();
    std::memcpy(b1.data(), "12345", b1.size());
    EXPECT_EQ(str_view{"12345"}, b1.view());
    EXPECT_EQ(str_view{"hello"}, b2.view());
}

TEST(CharBuf, Compare) {
    auto b0 = str_view{"abc"};
    auto b1 = str_view{"abc"};
    auto b2 = str_view{"abc_m"};
    auto b3 = str_view{"abc_n"};

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
    auto b1 = str_view{"kit cat"};
    auto b2 = b1;
    b2.move(4);
    EXPECT_EQ(str_view{"cat"}, b2);
    EXPECT_EQ(str_view{"t c"}, b1.substr(2, 3));
}

TEST(StringTable, Test) {
    details::string_table st;
    auto id1 = st.insert(str_view{"hello"});
    auto id2 = st.insert(str_view{"world"});
    EXPECT_EQ(str_view{"hello"}, st.string(id1));
    EXPECT_EQ(str_view{"world"}, st.string(id2));
}

TEST(TokenCursor, Test) {
    using details::token;

    auto code = str_view{
            "let x 10\n"
            "let y \"hello\"\n"
    };
    details::token_cursor tk{code};
    EXPECT_TRUE(tk.valid());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::kw_let);
    EXPECT_EQ(str_view{"let"}, tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::symbol);
    EXPECT_EQ(str_view{"x"}, tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::literal_int);
    EXPECT_EQ(str_view{"10"}, tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::line_end);
    EXPECT_EQ(str_view{"\n"}, tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::kw_let);
    EXPECT_EQ(str_view{"let"}, tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::symbol);
    EXPECT_EQ(str_view{"y"}, tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::literal_str);
    EXPECT_EQ(str_view{"hello"}, tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::line_end);
    EXPECT_EQ(str_view{"\n"}, tk.text());

    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::end);
    EXPECT_TRUE(tk.valid());
    // When end is reached, we stay in this state.
    EXPECT_TRUE(tk.next());
    EXPECT_TRUE(tk.get() == token::end);
    EXPECT_TRUE(tk.valid());
}

TEST(TPP, Compile) {
    auto program = compile_template(str_view{
            "let x 10\n"
            "let y \"hello\"\n"
    });
    ASSERT_TRUE(program);
    string_stream ss;
    program->run(&ss);
    auto s = ss.release();
    std::cout<<s<<std::endl;
}
}
}
