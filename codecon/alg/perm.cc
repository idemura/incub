#include <algorithm>
#include <map>
#include <stdio.h>
#include <string>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

using namespace std;

template <class Iter>
Iter getDescending(Iter first, Iter last) {
    auto i = first + 1;
    for (; i != last; ++i) {
        if (*(i - 1) < *i) {
            break;
        }
    }
    return i;
}

template <class Iter>
Iter getDescendingInsert(Iter first, Iter last, Iter x) {
    auto i = first;
    for (; i != last; ++i) {
        if (*i < *x) {
            break;
        }
    }
    return i;
}

// Gets all permutations.  If you want all, you should start with sequence
// ordered with <.  Final sequence ordered with >.  No equal items allowed.
// If no permutation generated, returns false.
template <class Iter>
bool perm(Iter first, Iter last) {
    auto desc_last = getDescending(first, last);
    if (desc_last == last) {
        return false; // Permutations done.
    }
    // Find where to insert to maintain descending order. `getDescendingInsert`
    // can't return `desc_last` - otherwise it will be counted by
    // `getDescending`.
    std::swap(*getDescendingInsert(first, last, desc_last), *desc_last);
    std::reverse(first, desc_last);
    return true;
}

void print(int *as, int an) {
    for (int i = 0; i < an; i++) {
        printf("%d ", as[i]);
    }
    printf("\n");
}

int main() {
    int as[] = {1, 2, 3, 4};
    int an = ARRAY_SIZEOF(as);
    int total = 0;
    do {
        total++;
        print(as, an);
    } while (perm(as, as + an));
    printf("Total: %d\n", total);
    return 0;
}
