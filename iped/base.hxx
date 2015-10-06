#ifndef IPED_BASE_HXX
#define IPED_BASE_HXX

#include <algorithm>
#include <iostream>
#include <functional>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace iped {

void flag_init();
bool add_flag(const char *name, std::string *f);

}  // namespace

#endif

