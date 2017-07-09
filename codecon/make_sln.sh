#! /bin/sh

[ "$#" -eq 1 ] || exit 1

cat > "$1".cc <<EOF
#include <cmath>
#include <cstdio>
#include <array>
#include <algorithm>
#include <map>
#include <vector>
#include <string>
#include <unordered_map>
#include <utility>
#include <iostream>

#include "logger.h"

using namespace std;

using i64 = long long int;
using pii = std::pair<int, int>;

int main() {

    return 0;
}
EOF

touch "$1".in

echo "./build.sh $1.cc && ./$1 < $1.in" | pbcopy
echo "Build and run command line is copied"
