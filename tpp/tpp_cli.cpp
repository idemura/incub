#include "tpp.hpp"

#include <unistd.h>

namespace idemura {
char_buf read_stdin() {
    std::vector<char_buf> pieces;
    uint32_t size = 0;
    for (;;) {
        char_buf buf{4096};
        auto num_bytes = ::read(0, buf.data(), buf.size());
        if (num_bytes < 0) {
            fatal("FATAL: fail to read stdin\n");
        }
        if (num_bytes == 0) {
            break;
        }
        size += num_bytes;
        auto sub_buf = buf.substr(0, num_bytes);
        if (num_bytes < 4096) {
            sub_buf = sub_buf.copy();
        }
        pieces.emplace_back(std::move(sub_buf));
    }
    char_buf big_buf{size};
    uint32_t ofs = 0;
    for (auto &b : pieces) {
        std::memcpy(big_buf.data() + ofs, b.data(), b.size());
        ofs += b.size();
    }
    return big_buf;
}
}

int main(int argc, char **argv) {
    using namespace idemura;

    auto program = compile_template(read_stdin());
    if (!program) {
        return 1;
    }
    auto text = program->run();
    ::write(1, text.data(), text.size());
    return 0;
}
