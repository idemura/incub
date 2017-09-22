#include "tpp.hpp"

#include <ostream>
#include <gtest/gtest.h>

namespace idemura {
std::ostream &operator<<(std::ostream &os, char_buf const &cb) {
    return os << to_string(cb);
}

namespace tests {
TEST(CharBuf, Basic) {
}

TEST(StringTable, Basic) {
    details::string_table st;
    auto id1 = st.insert(char_buf::strz("hello"));
    auto id2 = st.insert(char_buf::strz("world"));
    EXPECT_EQ(char_buf::strz("hello"), st.string(id1));
    EXPECT_EQ(char_buf::strz("world"), st.string(id2));
}

TEST(TPP, Compile) {
    auto program = compile_template(char_buf::strz(
            "let x 10\n"
            "let y \"hello\"\n"
    ));
    ASSERT_TRUE(program);
}
}
}
