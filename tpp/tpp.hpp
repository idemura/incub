#pragma once

#include "tpp_lib.hpp"

#include <map>

namespace idemura {
void set_error_file(int fd);

void error(char const *msg_fmt, ...);

[[noreturn]]
void fatal(char const *msg);

class output_stream {
public:
    virtual ~output_stream() = default;
    virtual void write(str_view buf) = 0;
};

class program {
public:
    virtual ~program() = default;
    virtual void run(output_stream *os) = 0;
};

std::unique_ptr<program> compile_template(str_view code);

class string_stream: public output_stream {
public:
    TPP_MOVE_NO_COPY(string_stream);

    string_stream() = default;
    void write(str_view buf) override;

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
    sys_call,
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

    string_id insert(str_view s);
    str_view string(string_id id);

private:
    std::map<str_view, string_id> index_;
    std::vector<char> text_;
};

class token_cursor {
public:
    TPP_COPY(token_cursor);

    explicit token_cursor(str_view code): code_{std::move(code)} {}

    bool valid() const {
        return token_ != token::invalid;
    }

    token get() {
        return token_;
    }

    str_view text() const {
        return text_;
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
    str_view code_;
    str_view text_;  // Text associated with the token
};

class function {
public:
    virtual ~function() = default;
    virtual str_view name() const = 0;
    virtual void invoke(std::vector<uint32_t> &stack) = 0;
};

class compiler {
public:
    TPP_MOVE_NO_COPY(compiler);

    explicit compiler(str_view code): cursor_{code} {}
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
