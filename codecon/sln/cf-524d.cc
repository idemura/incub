#include <algorithm>
#include <ctype.h>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <queue>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C)                                                        \
    C(const C &);                                                              \
    C &operator=(const C &);

using namespace std;

typedef long long int i64;

constexpr int INF = 0x7fffffff;
constexpr int DIM = 108;

// Timestamp is hh:mm:ss
void read_timestamps(const char *sp, int cn, int n, vector<int> &timestamps) {
    int h, m, s, moved;
    for (; n > 0; n--) {
        sscanf(sp, "%d:%d:%d%n", &h, &m, &s, &moved);
        timestamps.push_back(h * 3600 + m * 60 + s);
        sp += moved;
        cn -= moved;
    }
}

// The trick here is to keep the last one online more - because first will
// go offline sooner. When we "keep" the client, we should know is this the
// last instance of him online or not.
int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    int n, M, T;
    cin >> n >> M >> T;
    stringbuf sb;
    cin.get(sb, 0);
    auto buf = sb.str();
    vector<int> timestamps;
    read_timestamps(buf.data(), buf.size(), n, timestamps);
    int id = 1;
    vector<int> ids;
    vector<int> last_occurence(timestamps.size(), 1);
    int online = 0, online_first = 0;
    auto reached = false;
    for (int i = 0; i < timestamps.size();) {
        if (online > 0 && timestamps[online_first] + T <= timestamps[i]) {
            if (last_occurence[online_first]) online--;
            online_first++;
        } else {
            if (online == M) {
                last_occurence[i - 1] = 0;
                ids.push_back(id - 1);
            } else {
                ids.push_back(id++);
                online++;
                if (online == M) reached = true;
            }
            i++;
        }
    }
    if (reached) {
        cout << id - 1 << endl;
        for (auto i : ids) {
            cout << i << '\n';
        }
    } else {
        cout << "No solution" << endl;
    }
    return 0;
}
