#include "tpp.hpp"

#include <ostream>
#include <gtest/gtest.h>

namespace idemura {
std::ostream &operator<<(std::ostream &os, char_buf const &cb) {
    return os << to_string(cb);
}

namespace tests {
TEST(char_buf, basic) {
}

TEST(string_table, basic) {
    details::string_table st;
    auto id1 = st.insert(char_buf::strz("hello"));
    auto id2 = st.insert(char_buf::strz("world"));
    EXPECT_EQ(char_buf::strz("hello"), st.string(id1));
    EXPECT_EQ(char_buf::strz("world"), st.string(id2));
}

TEST(tpp, compile) {
    auto program = compile_template(char_buf::strz(
            "let x 10\n"
            "let y \"hello\"\n"
    ));
    ASSERT_TRUE(program);
}
}
}
