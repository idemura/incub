#pragma once

#include <cstdint>
#include <cstring>
#include <memory>
#include <utility>
#include <vector>

namespace idemura {
class char_buf {
public:
    static char_buf wrap(uint32_t size, char const *data) {
        return char_buf{size, data, false};
    }

    static char_buf wrap_strz(char const *data) {
        return char_buf{uint32_t(std::strlen(data)), data, false};
    }

    explicit char_buf(uint32_t size, char const *data = nullptr):
        char_buf(size, data, true) {}

    ~char_buf() {
        reset();
    }

    char_buf(): char_buf{0, nullptr, false} {}

    char_buf(char_buf &&other) noexcept:
        size_{other.size_},
        owns_{other.owns_},
        data_{other.data_} {}

    char_buf &operator=(char_buf &&other) {
        reset();
        size_ = other.size_;
        owns_ = other.owns_;
        data_ = other.data_;
        other.size_ = 0;
        other.data_ = nullptr;
        return *this;
    }

    void reset() {
      if (owns_) {
          delete[] data_;
      }
      size_ = 0;
      data_ = nullptr;
    }

    char_buf wrap() const {
        return char_buf{size_, data_, false};
    }

    void move(int delta) {
        data_ += delta;
        size_ -= delta;
    }

    bool none() const {
        return data_ == nullptr;
    }

    char_buf copy() const {
        return char_buf{size_, data_};
    }

    uint32_t size() const {
        return size_;
    }

    char *data() const {
        return data_;
    }

    // Returns a wrap, use @copy on it if want a duplicate.
    char_buf substr(uint32_t first, uint32_t count) const {
        if (first + count > size_) {
            count = size_ - first;
        }
        return char_buf{count, data_ + first, false};
    }

private:
    char_buf(uint32_t size, char const *data, bool owns):
        size_{size},
        owns_{owns},
        data_{new char[size]()} {
        if (data) {
            std::memcpy(data_, data, size);
        }
    }

    char_buf(char_buf const&) = delete;
    char_buf &operator=(char_buf const&) = delete;

    uint32_t size_{};
    bool owns_{true};
    char *data_{};
};

class program {
public:
    virtual ~program() = default;
    virtual char_buf run() = 0;
};

std::unique_ptr<program> compile_template(char_buf code);
void set_error_file(int fd);

namespace details {
enum class opcode: uint32_t {
    leti,
    lets,
    print,
};

enum class token: uint32_t {
    invalid,
    kw_let,
    symbol,
    literal_int,
    literal_str,
    line_end,
    print_begin,
    print_end,
    end,
};

class bytecode_gen {
public:
    bytecode_gen() = default;
    bytecode_gen(bytecode_gen const&) = delete;
    bytecode_gen &operator=(bytecode_gen const&) = delete;

    void add(opcode op) {
        bc_.push_back(static_cast<uint32_t>(op));
    }

    void add_arg(uint32_t arg) {
        bc_.push_back(arg);
    }

private:
    std::vector<uint32_t> bc_;
};

class token_cursor {
public:
    explicit token_cursor(char_buf code): code_{std::move(code)} {}
    token_cursor(token_cursor const &) = default;
    token_cursor &operator=(token_cursor const &) = default;

    token get() {
        return token_;
    }

    char_buf text() const {
        return text_.wrap();
    }

    uint32_t line() const {
        return line_ + 1;
    }

    bool next();

    bool valid() const {
        return token_ != token::invalid;
    }

private:
    void skip_space();
    uint32_t count_alnum();
    bool take_string();

    void invalidate() {
        token_ = token::invalid;
    }

    token token_{token::end};
    uint32_t line_{};
    char_buf code_;
    char_buf text_;  // Text associated with the token
};

class compiler {
public:
    explicit compiler(char_buf code): cursor_{code.wrap()} {}
    std::unique_ptr<program> compile();

private:
    bool compile_let();

    token_cursor cursor_;
    bytecode_gen bc_;
};

class program_impl: public program {
public:
    program_impl() {}
    program_impl(program_impl const&) = delete;
    program_impl &operator=(program_impl const&) = delete;
    ~program_impl() override = default;

    char_buf run() override;

private:
    std::vector<uint32_t> bc_;
};
}
}
