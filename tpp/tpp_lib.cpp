#include "tpp_lib.hpp"

namespace idemura {
std::string to_string(str_view sv) {
    return std::string{sv.data(), sv.size()};
}
}
