#ifndef IGOR_PARSER_HXX
#define IGOR_PARSER_HXX

#include "base.hxx"

namespace igor {
bool parse(std::istream *in, std::function<void(const string&)> error_fn);
}  // namespace

#endif
