#pragma once

#include <cstdint>
#include <cstring>
#include <algorithm>
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
// TODO(C++17): Use std
template<class T, size_t N>
constexpr size_t size(const T (&array)[N]) noexcept {
    return N;
}

enum class compare_t {
    lt,
    eq,
    gt,
};

class str_view {
public:
    str_view(): str_view{nullptr, 0} {}
    explicit str_view(char const *data): str_view(data, std::strlen(data)) {}

    str_view(char const *data, uint32_t size):
        data_{data},
        size_{size} {}

    void reset() {
      data_ = nullptr;
      size_ = 0;
    }

    void move(int delta) {
        data_ += delta;
        size_ -= delta;
    }

    char const *data() const {
        return data_;
    }

    uint32_t size() const {
        return size_;
    }

    void copy_to(char *dest) const {
        std::memcpy(dest, data_, size_);
    }

    // Returns a wrap, use @copy on it if want a duplicate.
    str_view substr(uint32_t first, uint32_t count) const {
        if (first + count > size_) {
            count = size_ - first;
        }
        return str_view{data_ + first, count};
    }

    bool equals(char const *strz) const {
        return std::strlen(strz) == size_ && std::memcmp(data_, strz, size_) == 0;
    }

    compare_t compare(str_view other) const {
        auto c = std::memcmp(data_, other.data_, std::min(size_, other.size_));
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
    char const *data_{};
    uint32_t size_{};
};

inline bool operator<(str_view l, str_view r) {
    return l.compare(r) == compare_t::lt;
}

inline bool operator<=(str_view l, str_view r) {
    return l.compare(r) != compare_t::gt;
}

inline bool operator>(str_view l, str_view r) {
    return l.compare(r) == compare_t::gt;
}

inline bool operator>=(str_view l, str_view r) {
    return l.compare(r) != compare_t::lt;
}

inline bool operator==(str_view l, str_view r) {
    return l.compare(r) == compare_t::eq;
}

inline bool operator!=(str_view l, str_view r) {
    return l.compare(r) != compare_t::eq;
}

std::string to_string(str_view cb);

class char_buf {
public:
    static char_buf wrap(uint32_t size, char *data) {
        return char_buf{size, data};
    }

    static char_buf strz(char const *data) {
        return char_buf{uint32_t(std::strlen(data)), data};
    }

    char_buf(): char_buf{0, nullptr} {}

    explicit char_buf(uint32_t size, char const *data = nullptr):
        data_{new char[size]()},
        size_{size} {
        if (data) {
            std::memcpy(data_, data, size);
        }
    }

    char_buf(char_buf &&other) noexcept:
        data_{other.data_},
        size_{other.size_} {
        other.size_ = 0;
        other.data_ = nullptr;
    }

    char_buf &operator=(char_buf &&other) {
        reset();
        size_ = other.size_;
        data_ = other.data_;
        other.size_ = 0;
        other.data_ = nullptr;
        return *this;
    }

    ~char_buf() {
        reset();
    }

    void reset() {
      delete[] data_;
      data_ = nullptr;
      size_ = 0;
    }

    str_view view() const {
        return str_view{data_, size_};
    }

    char *data() const {
        return data_;
    }

    uint32_t size() const {
        return size_;
    }

    char_buf copy() const {
        return char_buf{size_, data_};
    }

    void copy_to(char *dest) const {
        std::memcpy(dest, data_, size_);
    }

private:
    TPP_NO_COPY(char_buf);

    char *data_{};
    uint32_t size_{};
};
}
