#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <map>
#include <set>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i32 = int32_t;
using i64 = int64_t;
using pii = std::pair<int, int>;

struct block {
    block(int n, int i): n{n}, i{i} {}
    int n{};
    int i{};
    int length{};

    block *prev{nullptr};
    block *next{nullptr};

    void unlink() {
        if (prev) {
            prev->next = next;
        }
        if (next) {
            next->prev = prev;
        }
    }
};

struct block_cmp {
    bool operator()(block const *a, block const *b) const {
        if (a->length == b->length) {
            return a->i < b->i;
        } else {
            return a->length > b->length;
        }
    }
};

int main() {
    std::set<block *, block_cmp> order;
    int n;
    scanf("%d", &n);
    block *prev = nullptr;
    int l = 0;
    int i = 0;
    for (; i < n; i++) {
        int x = 0;
        scanf("%d", &x);
        if (!prev || prev->n != x) {
            auto b = new block{x, i};
            b->prev = prev;
            if (prev) {
                prev->length = i - prev->i;
                prev->next = b;
                order.insert(prev);
            }
            prev = b;
            l = 0;
        }
        l++;
    }
    if (prev) {
        prev->length = i - prev->i;
        order.insert(prev);
    }
    int step = 0;
    while (!order.empty()) {
        step++;
        auto p = *order.begin();
        if (p->next && p->prev && p->next->n == p->prev->n) {
            order.erase(p->next);
            order.erase(p->prev);
            p->prev->length += p->next->length;
            p->next->unlink();
            order.insert(p->prev);
        }
        order.erase(p);
        p->unlink();
    }
    printf("%d\n", step);
    return 0;
}
