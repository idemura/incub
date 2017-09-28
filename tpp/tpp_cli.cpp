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
        if (buf.size() < 4096) {
            buf = char_buf{buf.size(), buf.data()};
        }
        pieces.emplace_back(std::move(buf));
    }
    char_buf big_buf{size};
    uint32_t ofs = 0;
    for (auto &b : pieces) {
        std::memcpy(big_buf.data() + ofs, b.data(), b.size());
        ofs += b.size();
    }
    return big_buf;
}

class file_stream: public output_stream {
public:
    explicit file_stream(int fd): fd_{fd} {}

    void write(str_view buf) override {
        ::write(fd_, buf.data(), buf.size());
    }

private:
    int const fd_{};
};
}

int main(int argc, char **argv) {
    using namespace idemura;

    auto code = read_stdin();
    auto program = compile_template(code.view());
    if (!program) {
        return 1;
    }
    auto stream = std::make_unique<file_stream>(1);
    program->run(stream.get());
    return 0;
}
