#include "tpp.hpp"

#include <unistd.h>
#include <cassert>
#include <cctype>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>

namespace idemura {
namespace details {
static int fd_error{2};

std::string to_string(token t) {
    switch (t) {
        case token::invalid:
            return "invalid";
        case token::kw_let:
            return "let";
        case token::symbol:
            return "symbol";
        case token::literal_int:
            return "int";
        case token::literal_str:
            return "string";
        case token::line_end:
            return "line_end";
        case token::print_begin:
            return "[``";
        case token::print_end:
            return "``]";
        case token::end:
            return "end";
    }
    fatal("to_string: unknown token");
}

string_id string_table::insert(char_buf s) {
    auto where = index_.find(s);
    if (where != index_.end()) {
        return where->second;
    }

    auto id = string_id(text_.size(), s.size());
    text_.resize(id.offset + id.size);
    s.copy_to(text_.data() + id.offset);

    index_.emplace(std::move(s), id);
    return id;
}

char_buf string_table::string(string_id id) {
    assert(id.offset <= text_.size());
    assert(id.offset + id.size <= text_.size());
    return char_buf::wrap(id.size, text_.data() + id.offset);
}

uint32_t token_cursor::count_alnum() {
    auto n = code_.size();
    auto p = code_.data();
    uint32_t i = 0;
    while (i < n && (std::isalnum(p[i]) || p[i] == '_')) {
        i++;
    }
    return i;
}

void token_cursor::skip_space() {
    auto n = code_.size();
    auto p = code_.data();
    uint32_t i = 0;
    while (i < n && std::isspace(p[i]) && p[i] != '\n') {
        i++;
    }
    code_.move(i);
}

bool token_cursor::take_string() {
    auto n = code_.size();
    auto p = code_.data();
    uint32_t i = 1;
    while (i < n && p[i] != p[0]) {
        if (!(32 <= p[i] && p[i] < 127)) {
            invalidate();
            return false;
        }
        i++;
    }
    token_ = token::literal_str;
    text_ = code_.substr(1, i - 1);
    i++;  // Skip ending quote
    code_.move(i);
    return true;
}

bool token_cursor::next() {
    if (!valid()) {
        return false;
    }

    text_.reset();

    skip_space();
    if (code_.size() == 0) {
        token_ = token::end;  // Just in case of invalid token
        return true;
    }

    auto n_alnum = count_alnum();
    if (n_alnum > 0) {
        text_ = code_.substr(0, n_alnum);
        auto p = code_.data();
        if (std::isdigit(*p)) {
            token_ = token::literal_int;
        } else {
            if (std::strncmp(code_.data(), "let", n_alnum) == 0) {
                token_ = token::kw_let;
            } else {
                token_ = token::symbol;
            }
        }
        code_.move(n_alnum);
        return true;
    }

    uint32_t tok_len = 0;
    switch (*code_.data()) {
        case '\n': {
            token_ = token::line_end;
            tok_len = 1;
            line_++;
            break;
        }
        case '"':
            return take_string();
        case '`':
            // return take_print();
            fatal("not implemented");
            break;
        default: {
            // Do not move, just mark invalid.
            invalidate();
            return false;
        }
    }
    text_ = code_.substr(0, tok_len);
    code_.move(tok_len);
    return true;
}

void compiler::compile_error(char const *msg_fmt, ...) {
    char buf[120];
    std::va_list va;
    va_start(va, msg_fmt);
    std::vsnprintf(buf, sizeof buf, msg_fmt, va);
    va_end(va);
    error("line %u: %s\n", cursor_.line(), buf);
    error_count_++;
}

std::unique_ptr<program> compiler::compile() {
    for (auto stop = false; !stop;) {
        if (!cursor_.next()) {
            break;
        }
        switch (cursor_.get()) {
            case token::end: {
                stop = true;
                break;
            }
            case token::kw_let: {
                stop = compile_let();
                break;
            }
            case token::symbol: {
                stop = compile_expression();
                break;
            }
            default: {
                compile_error(
                        "unexpected token type %s",
                        to_string(cursor_.get()).c_str());
                stop = true;
            }
        }
    }
    if (!cursor_.valid()) {
        compile_error("invalid token");
    }
    if (error_count_ > 0) {
        bc_ = bytecode_gen{};
        st_ = string_table{};
        return nullptr;
    }
    return std::make_unique<details::program_impl>(
            std::move(bc_),
            std::move(st_));
}

bool compiler::compile_let() {
    if (!cursor_.next()) {
        return false;
    }
    if (cursor_.get() != token::symbol) {
        compile_error("'let' name: symbol expected");
        return false;
    }
    auto name = cursor_.text();
    if (!cursor_.next()) {
        return false;
    }
    char_buf value;
    switch (cursor_.get()) {
        case token::literal_int: {
            value = cursor_.text();
            break;
        }
        case token::literal_str: {
            value = cursor_.text();
            break;
        }
        default: {
            compile_error("'let': expression expected");
            return false;
        }
    }
    if (!cursor_.next()) {
        return false;
    }
    if (cursor_.get() != token::line_end) {
        compile_error("'let': line end expected");
        return false;
    }
    return true;
}

bool compiler::compile_expression() {
    auto fn_name = cursor_.text();
    if (!cursor_.next()) {
        return false;
    }
    if (!fn_name.equals("out")) {
        fatal("not supported");
    }
    bc_.add_op(opcode::call);
    bc_.add_str(st_.insert(fn_name));
    while (cursor_.valid() && cursor_.get() != token::line_end) {
        switch (cursor_.get()) {
            case token::symbol: {
                fatal("not supported");
            }
            case token::literal_int: {
                char buf[24];
                auto text = cursor_.text();
                if (text.size() >= sizeof buf) {
                    compile_error("int literal too large");
                    return false;
                }
                text.copy_to(buf);
                int64_t n = 0;
                int consumed = 0;
                if (std::sscanf(buf, "%ld%n", &n, &consumed) != 1 || consumed != text.size()) {
                    compile_error("int literal too large");
                    return false;
                }
                bc_.add_int(n);
                break;
            }
            case token::literal_str: {
                fatal("not supported");
                break;
            }
            default: {
                compile_error("expression: arguments must be literals");
                return false;
            }
        }
    }
    return cursor_.valid();
}

char_buf program_impl::run() {
    // std::vector<
    for (uint32_t i = 0; i < bc_.size();) {
        switch (static_cast<opcode>(bc_[i])) {
            case token::call: {
                break;
            }
            default: {
                fatal("unsupported op code");
            }
        }
        i++;
    }
}
}

std::string to_string(char_buf const &buf) {
    return std::string{buf.data(), buf.size()};
}

void error(char const *msg_fmt, ...) {
    if (details::fd_error <= 0) {
        return;
    }
    char buf[120];
    std::va_list va;
    va_start(va, msg_fmt);
    std::vsnprintf(buf, sizeof buf, msg_fmt, va);
    va_end(va);
    ::write(details::fd_error, buf, std::strlen(buf));
}

[[noreturn]]
void fatal(char const *msg) {
    error(msg);
    error("\nabort\n");
    std::abort();
}

std::unique_ptr<program> compile_template(char_buf code) {
    details::compiler c{code.wrap()};
    return c.compile();
}

void set_error_file(int fd) {
    details::fd_error = fd;
}
}
