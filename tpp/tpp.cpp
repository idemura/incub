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

uint64_t string_table::insert(char_buf s) {
    auto where = index_.find(s);
    if (where != index_.end()) {
        return where->second.encode();
    }

    auto id = id_t(text_.size(), s.size());
    text_.resize(id.offset + id.size);
    s.copy_to(text_.data() + id.offset);

    index_.emplace(std::move(s), id);
    return id.encode();
}

char_buf string_table::string(uint64_t id) {
    id_t decoded{id};
    return char_buf::wrap(decoded.size, text_.data() + decoded.offset);
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
                if (!compile_let()) {
                    stop = true;
                }
                break;
            }
            case token::symbol: {
                break;
            }
            case token::literal_int: {
                break;
            }
            default: {
                fatal("FATAL: unknown token type");
            }
        }
    }
    if (!cursor_.valid()) {
        error("invalid token on line %u\n", cursor_.line());
        return nullptr;
    }
    return std::make_unique<details::program_impl>();
}

bool compiler::compile_let() {
    if (!cursor_.next()) {
        return false;
    }
    if (cursor_.get() != token::symbol) {
        error("line %u: let: symbol expected", cursor_.line());
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
            error("line %u: let: literal expected\n", cursor_.line());
            return false;
        }
    }
    if (!cursor_.next()) {
        return false;
    }
    if (cursor_.get() != token::line_end) {
        error("line %u: let: end of line expected\n", cursor_.line());
        return false;
    }
    return true;
}

char_buf program_impl::run() {
    uint32_t i = 0;
    while (i < bc_.size()) {
        // switch () {
        // }
        i++;
    }
    return char_buf::strz("dummy\n");
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
