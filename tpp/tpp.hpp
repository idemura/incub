#pragma once

#include <cstdint>
#include <cstring>
#include <algorithm>
#include <map>
#include <memory>
#include <utility>
#include <vector>
#include <string>

#define TPP_CTOR_ASSIGN(Class, Keyword, ArgSpec) \
    Class(Class ArgSpec) = Keyword; \
    Class &operator=(Class ArgSpec) = Keyword;

#define TPP_COPY(Class) TPP_CTOR_ASSIGN(Class, default, const&)
#define TPP_NO_COPY(Class) TPP_CTOR_ASSIGN(Class, delete, const&)
#define TPP_MOVE(Class) TPP_CTOR_ASSIGN(Class, default, &&)
#define TPP_NO_MOVE(Class) TPP_CTOR_ASSIGN(Class, delete, &&)

#define TPP_MOVE_NO_COPY(Class) TPP_MOVE(Class) TPP_NO_COPY(Class)

namespace idemura {
// @char_buf and utilities
//
enum class compare_t {
    lt,
    eq,
    gt,
};

class char_buf {
public:
    static char_buf wrap(uint32_t size, char *data) {
        return char_buf{size, data, no_ownership()};
    }

    static char_buf strz(char const *data) {
        return char_buf{uint32_t(std::strlen(data)), data};
    }

    char_buf(): char_buf{0, nullptr, no_ownership()} {}

    explicit char_buf(uint32_t size, char const *data = nullptr):
        size_{size},
        owns_{true},
        data_{new char[size]()} {
        if (data) {
            std::memcpy(data_, data, size);
        }
    }

    char_buf(char_buf &&other) noexcept:
        size_{other.size_},
        owns_{other.owns_},
        data_{other.data_} {
        other.size_ = 0;
        other.data_ = nullptr;
    }

    char_buf &operator=(char_buf &&other) {
        reset();
        size_ = other.size_;
        owns_ = other.owns_;
        data_ = other.data_;
        other.size_ = 0;
        other.data_ = nullptr;
        return *this;
    }

    ~char_buf() {
        reset();
    }

    void reset() {
      if (owns_) {
          delete[] data_;
      }
      size_ = 0;
      data_ = nullptr;
    }

    char_buf wrap() const {
        return char_buf{size_, data_, no_ownership()};
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

    void copy_to(char *dest) const {
        std::memcpy(dest, data_, size_);
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
        return char_buf{count, data_ + first, no_ownership()};
    }

    bool equals(char const *strz) const {
        return std::strlen(strz) == size_ && std::memcmp(data_, strz, size_) == 0;
    }

    compare_t compare(char_buf const &other) const {
        auto len = std::min(size_, other.size_);
        auto c = std::memcmp(data_, other.data_, len);
        if (c < 0) {
            return compare_t::lt;
        }
        if (c > 0) {
            return compare_t::gt;
        }
        if (size_ < other.size_) {
            return compare_t::lt;
        }
        if (size_ > other.size_) {
            return compare_t::gt;
        }
        return compare_t::eq;
    }

private:
    TPP_NO_COPY(char_buf);

    struct no_ownership {};

    char_buf(uint32_t size, char *data, no_ownership):
        size_{size},
        owns_{false},
        data_{data} {}

    uint32_t size_{};
    bool owns_{true};
    char *data_{};
};

inline bool operator<(char_buf const &l, char_buf const &r) {
    return l.compare(r) == compare_t::lt;
}

inline bool operator<=(char_buf const &l, char_buf const &r) {
    return l.compare(r) != compare_t::gt;
}

inline bool operator>(char_buf const &l, char_buf const &r) {
    return l.compare(r) == compare_t::gt;
}

inline bool operator>=(char_buf const &l, char_buf const &r) {
    return l.compare(r) != compare_t::lt;
}

inline bool operator==(char_buf const &l, char_buf const &r) {
    return l.compare(r) == compare_t::eq;
}

inline bool operator!=(char_buf const &l, char_buf const &r) {
    return l.compare(r) != compare_t::eq;
}

std::string to_string(char_buf const &buf);

//
// End @char_buf related code

void set_error_file(int fd);

void error(char const *msg_fmt, ...);

[[noreturn]]
void fatal(char const *msg);

class output_stream {
public:
    virtual ~output_stream() = default;
    virtual void write(char_buf buf) = 0;
};

class program {
public:
    virtual ~program() = default;
    virtual void run(output_stream *os) = 0;
};

std::unique_ptr<program> compile_template(char_buf code);

class string_stream: public output_stream {
public:
    TPP_MOVE_NO_COPY(string_stream);

    string_stream() = default;
    void write(char_buf buf) override;

    // Gets underlying string and replaces it with empty
    std::string release() {
        std::string res;
        std::swap(str_, res);
        return res;
    }

private:
    std::string str_;
};

namespace details {
enum class opcode: uint32_t {
    call,
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

struct string_id {
    uint32_t const offset{};
    uint32_t const size{};

    string_id(uint32_t o, uint32_t s): offset{o}, size{s} {}
};

// Collects strings and assigns them IDs.
class string_table {
public:
    TPP_MOVE_NO_COPY(string_table);

    string_table() = default;

    string_id insert(char_buf s);
    char_buf string(string_id id);

private:
    std::map<char_buf, string_id> index_;
    std::vector<char> text_;
};

class token_cursor {
public:
    TPP_COPY(token_cursor);

    explicit token_cursor(char_buf code): code_{std::move(code)} {}

    bool valid() const {
        return token_ != token::invalid;
    }

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
    TPP_MOVE_NO_COPY(compiler);

    explicit compiler(char_buf code): cursor_{code.wrap()} {}
    std::unique_ptr<program> compile();

private:
    bool compile_let();
    bool compile_expression();
    void compile_error(char const *msg_fmt, ...);

    void add_op(opcode op) {
        bc_.push_back(static_cast<uint32_t>(op));
    }

    void add_int(int64_t n) {
        bc_.push_back(n >> 32);
        // Coersion below will take 32 lowest bits
        bc_.push_back(n);
    }

    void add_str(string_id id) {
        bc_.push_back(id.offset);
        bc_.push_back(id.size);
    }

    token_cursor cursor_;
    uint32_t error_count_{};
    std::vector<uint32_t> bc_;
    string_table st_;
};

class program_impl: public program {
public:
    TPP_MOVE_NO_COPY(program_impl);

    program_impl(std::vector<uint32_t> bc, string_table st):
        bc_{std::move(bc)},
        st_{std::move(st)} {}

    void run(output_stream *os) override;

private:
    std::vector<uint32_t> bc_;
    string_table st_;
};
}
}
