#include <algorithm>
#include <assert.h>
#include <ctype.h>
#include <map>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

static const int kBase = 26;

typedef char *SStr;

// Least significant digit (LSD) first. Assumes every element has same `width`.
// This implementation uses vectors for buckets. Alternative can use a single
// array and count indices: see implementations below.
// Note all LSD modifications require additional memory of size of the input
// array.
// Sorting is stable.
void lsdRadixSort(SStr *a, int an, int width) {
    std::vector<SStr> bins[kBase];
    for (int n = width; n--;) {
        for (int i = 0; i < an; i++) {
            bins[a[i][n] - 'a'].push_back(a[i]);
        }
        // Fill back to input array.
        int k = 0;
        for (int i = 0; i < kBase; i++) {
            std::vector<SStr> &bin = bins[i];
            for (int j = 0; j < bin.size(); j++) {
                a[k++] = bin[j];
            }
            bin.clear();
        }
    }
}

// One single temp buffer and counting each elements. Bad thing: we has two
// runs over `a`.
void lsdRadixSortCountInc(SStr *a, int an, int width) {
    using std::swap;
    std::vector<SStr> temp(an);
    SStr *s = a, *d = &temp[0];
    for (int n = width; n--;) {
        int count[kBase + 1] = {};
        for (int j = 0; j < an; j++) {
            int c = s[j][n] - 'a';
            count[c + 1]++; // Note `c + 1`, count[0] will remain zero.
        }
        for (int j = 1; j < ARRAY_SIZEOF(count); j++) {
            count[j] += count[j - 1];
        }
        // In this implementation we place elements back head to tail. We have
        // to have additional counter(index), but we run from lower to higher
        // memory addresses that is positively on the cache.
        for (int j = 0; j < an; j++) {
            int k = count[s[j][n] - 'a']++;
            d[k] = s[j];
        }
        swap(d, s);
    }
    if (a != s) {
        for (int i = 0; i < an; i++) {
            a[i] = s[i];
        }
    }
}

// One single temp buffer and counting each elements. Bad thing: we has two
// runs over `a`.
void lsdRadixSortCountRev(SStr *a, int an, int width) {
    using std::swap; // ADL friendly way.
    std::vector<SStr> temp(an);
    SStr *s = a, *d = &temp[0];
    for (int n = width; n--;) {
        int count[kBase] = {};
        for (int j = 0; j < an; j++) {
            count[s[j][n] - 'a']++;
        }
        for (int j = 1; j < ARRAY_SIZEOF(count); j++) {
            count[j] += count[j - 1];
        }
        // In this implementation we place elements back tail to head and
        // `count` procedure is changed.
        for (int j = an; j--;) {
            d[--count[s[j][n] - 'a']] = s[j];
        }
        swap(d, s);
    }
    if (a != s) {
        for (int i = 0; i < an; i++) {
            a[i] = s[i];
        }
    }
}

// Works only for binary digits! For test we use '0' and '1'. Theoretically,
// it doesn't have any benefits over comparison sorts, because `an` requires at
// least `log an` bits. It's just for fun. It could be fixed with counting
// approach and having each "bit" more than 2 values, but I didn't implement it.
// In any case MSD radix sort is not stable, compared to LSD.
void msdRadixSortRec(SStr *a, int an, int r, int width) {
    using std::swap;
    if (r == width || an <= 1) {
        return;
    }
    int i = 0, j = an - 1;
    for (; i <= j;) {
        for (; a[i][r] == '0' && i <= j; i++) {
        }
        for (; a[j][r] == '1' && i <= j; j--) {
        }
        if (i < j) {
            swap(a[j], a[i]);
        }
    }
    msdRadixSortRec(a, i, r + 1, width);
    msdRadixSortRec(a + i, an - i, r + 1, width);
}

void printSStrArray(SStr *a, int an) {
    for (int i = 0; i < an; i++) {
        printf("%s\n", a[i]);
    }
}

void checkSort(SStr *a, int an) {
    for (int i = 1; i < an; i++) {
        assert(strcmp(a[i - 1], a[i]) <= 0);
    }
}

void testLSDSorts() {
    static const int width = 3;
    char a_charptr[][width + 1] = {
            "aba",
            "aaa",
            "aca",
            "aac",
            "aab",
            "acb",
            "abc",
            "bbc",
            "bac",
            "abc",
    };
    SStr a[ARRAY_SIZEOF(a_charptr)] = {};

    for (int i = 0; i < ARRAY_SIZEOF(a); i++) {
        a[i] = a_charptr[i];
    }
    printf("LSD radix sort, bucket\n");
    lsdRadixSort(a, ARRAY_SIZEOF(a), width);
    checkSort(a, ARRAY_SIZEOF(a));

    for (int i = 0; i < ARRAY_SIZEOF(a); i++) {
        a[i] = a_charptr[i];
    }
    printf("LSD radix sort, counting inc\n");
    lsdRadixSortCountInc(a, ARRAY_SIZEOF(a), width);
    checkSort(a, ARRAY_SIZEOF(a));

    for (int i = 0; i < ARRAY_SIZEOF(a); i++) {
        a[i] = a_charptr[i];
    }
    printf("LSD radix sort, counting rev\n");
    lsdRadixSortCountRev(a, ARRAY_SIZEOF(a), width);
}

void testMSDSorts() {
    static const int width = 3;
    char a_charptr[][width + 1] = {
            "000",
            "011",
            "010",
            "101",
            "111",
            "001",
            "110",
            "101",
            "001",
    };
    SStr a[ARRAY_SIZEOF(a_charptr)];

    for (int i = 0; i < ARRAY_SIZEOF(a); i++) {
        a[i] = a_charptr[i];
    }
    printf("MSD radix sort\n");
    msdRadixSortRec(a, ARRAY_SIZEOF(a), 0, width);
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    testLSDSorts();
    testMSDSorts();
    return 0;
}
