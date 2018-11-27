#! /bin/sh

[ "$#" -eq 1 ] || exit 1

cat > "$1".cc <<EOF
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "log.h"

using namespace std;

using i32 = int32_t;
using i64 = int64_t;
using pii = std::pair<int, int>;

int main(int argc, char **argv) {
    initLog(argc, argv);
    return 0;
}
EOF

touch "$1".in

echo "./build.sh $1.cc && ./$1 < $1.in" | pbcopy
echo "Build and run command line is copied"
