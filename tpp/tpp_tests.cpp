#include "tpp.hpp"

#include <cassert>
#include <cstdlib>
#include <iostream>

#define CHECK_WITH(Expr, Abort) \
    do { \
        ::idemura::testing::check( \
                !(Expr), \
                #Expr, \
                __FILE__, \
                __LINE__, \
                Abort); \
    } while (false)

#define EXPECT(Expr) CHECK_WITH(Expr, false)
#define ASSERT(Expr) CHECK_WITH(Expr, true)

namespace idemura {
namespace testing {
static uint32_t failed = 0;

void check(
        bool check_failed,
        char const *expr,
        char const *file,
        uint32_t line,
        bool abort) {
    if (check_failed) {
        failed++;
        std::cerr<<"Check failed: "<<expr<<" in "<<file<<":"<<line<<std::endl;
        if (abort) {
            std::abort();
        }
    }
}

[[noreturn]]
void exit() {
    if (failed > 0) {
        std::exit(1);
    } else {
        std::cerr<<"ALL TESTS PASSED\n";
        std::exit(0);
    }
}
}
}

namespace idemura {
void test_1() {
    auto program = compile_template(char_buf::strz(
            "let x 10\n"
            "let y \"hello\"\n"
    ));
    ASSERT(program);
}
}

int main(int argc, char **argv) {
    using namespace idemura;

    test_1();
    testing::exit();
}
