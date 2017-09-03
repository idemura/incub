#! /bin/sh

[ "$#" -eq 1 ] || exit 1

cat > "$1".cc <<EOF
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <algorithm>
#include <map>
#include <vector>
#include <string>
#include <unordered_map>
#include <utility>

#include "log.hpp"

using namespace std;

using i32 = int32_t;
using i64 = int64_t;
using pii = std::pair<int, int>;

int main() {
    return 0;
}
EOF

touch "$1".in

echo "./build.sh $1.cc && ./$1 < $1.in" | pbcopy
echo "Build and run command line is copied"
