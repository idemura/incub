#include <algorithm>
#include <cstdint>
#include <cstring>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "log.h"
#include <gtest/gtest.h>

// Vector consisting of several power-of-2 runs of sorted data.
// For example, LSM of size 6 consists of two runs, 4 and 2 specificly.
template <typename T>
class LSM {
public:
    using Value = T;

    LSM() = default;

    void reserve(size_t n) {
        lsm.reserve(n);
    }

    void push(Value v) {
        long n = lsm.size();
        lsm.push_back(v);
        if ((n & 1) == 0) {
            return;
        }
        long runSize = 1;
        ScopedBuf guard{lsm.size()};
        auto buf = guard.ptr;
        while ((n & 1) != 0) {
            std::memcpy(
                    buf, &lsm[lsm.size() - runSize], runSize * sizeof(Value));
            // Merge
            long src1First = lsm.size() - 2 * runSize;
            long src1Size = runSize - 1;
            long src2Size = runSize - 1;
            long dst = lsm.size() - 1;
            while (src1Size >= 0 && src2Size >= 0) {
                if (lsm[src1First + src1Size] > buf[src2Size]) {
                    lsm[dst--] = lsm[src1First + src1Size];
                    src1Size--;
                } else {
                    lsm[dst--] = buf[src2Size];
                    src2Size--;
                }
            }
            while (src1Size >= 0) {
                lsm[dst--] = lsm[src1First + src1Size];
                src1Size--;
            }
            while (src2Size >= 0) {
                lsm[dst--] = buf[src2Size];
                src2Size--;
            }
            n >>= 1;
            runSize *= 2;
        }
    }

    size_t countLess(Value max) const {
        size_t res = 0;
        long runSize = 1;
        long n = lsm.size();
        long s = 0;
        while (n > 0) {
            if (n & 1) {
                auto itFirst = lsm.end() - (s + runSize);
                res += std::lower_bound(itFirst, itFirst + runSize, max) -
                        itFirst;
                s += runSize;
            }
            n >>= 1;
            runSize *= 2;
        }
        return res;
    }

    size_t size() const {
        return lsm.size();
    }

    std::string toString() const {
        std::string s{"LSM: "};
        if (lsm.empty()) {
            s.append("<empty>");
            return s;
        }
        long r = 1;
        while (~(r | (r - 1)) & lsm.size()) {
            r <<= 1;
        }
        long c = 0;
        for (auto x : lsm) {
            s.append(std::to_string(x)).append(" ");
            c++;
            if (c == r) {
                do {
                    r >>= 1;
                } while (r > 0 && (r & lsm.size()) == 0);
                if (r > 0) {
                    s.append("| ");
                }
                c = 0;
            }
        }
        return s;
    }

    std::vector<Value> getCopy() const {
        return lsm;
    }

private:
    struct ScopedBuf {
        explicit ScopedBuf(size_t n):
                ptr{reinterpret_cast<Value *>(std::malloc(n * sizeof(Value)))} {
        }
        ScopedBuf(ScopedBuf const &) = delete;
        ScopedBuf &operator=(ScopedBuf const &) = delete;
        ~ScopedBuf() {
            std::free(ptr);
        }

        Value *const ptr{};
    };

    std::vector<Value> lsm;
};

TEST(LSM, Basic) {
    LSM<int> lsm;
    lsm.push(4);
    {
        auto a = lsm.getCopy();
        EXPECT_EQ(1, a.size());
        EXPECT_EQ(4, a[0]);
    }
    lsm.push(1);
    {
        auto a = lsm.getCopy();
        EXPECT_EQ(2, a.size());
        EXPECT_EQ(1, a[0]);
        EXPECT_EQ(4, a[1]);
    }
    lsm.push(2);
    {
        auto a = lsm.getCopy();
        EXPECT_EQ(3, a.size());
        EXPECT_EQ(1, a[0]);
        EXPECT_EQ(4, a[1]);
        EXPECT_EQ(2, a[2]);
    }
    lsm.push(3);
    {
        auto a = lsm.getCopy();
        EXPECT_EQ(4, a.size());
        EXPECT_EQ(1, a[0]);
        EXPECT_EQ(2, a[1]);
        EXPECT_EQ(3, a[2]);
        EXPECT_EQ(4, a[3]);
    }
    lsm.push(6);
    {
        auto a = lsm.getCopy();
        EXPECT_EQ(5, a.size());
        EXPECT_EQ(1, a[0]);
        EXPECT_EQ(2, a[1]);
        EXPECT_EQ(3, a[2]);
        EXPECT_EQ(4, a[3]);
        EXPECT_EQ(6, a[4]);
        EXPECT_EQ("LSM: 1 2 3 4 | 6 ", lsm.toString());
    }
    lsm.push(9);
    {
        auto a = lsm.getCopy();
        EXPECT_EQ(6, a.size());
        EXPECT_EQ(1, a[0]);
        EXPECT_EQ(2, a[1]);
        EXPECT_EQ(3, a[2]);
        EXPECT_EQ(4, a[3]);
        EXPECT_EQ(6, a[4]);
        EXPECT_EQ(9, a[5]);
        EXPECT_EQ("LSM: 1 2 3 4 | 6 9 ", lsm.toString());
    }
    lsm.push(4);
    {
        auto a = lsm.getCopy();
        EXPECT_EQ(7, a.size());
        EXPECT_EQ(1, a[0]);
        EXPECT_EQ(2, a[1]);
        EXPECT_EQ(3, a[2]);
        EXPECT_EQ(4, a[3]);
        EXPECT_EQ(6, a[4]);
        EXPECT_EQ(9, a[5]);
        EXPECT_EQ(4, a[6]);
        EXPECT_EQ("LSM: 1 2 3 4 | 6 9 | 4 ", lsm.toString());
    }
    EXPECT_EQ(0, lsm.countLess(1));
    EXPECT_EQ(1, lsm.countLess(2));
    EXPECT_EQ(2, lsm.countLess(3));
    EXPECT_EQ(3, lsm.countLess(4));
    EXPECT_EQ(5, lsm.countLess(5));
    EXPECT_EQ(5, lsm.countLess(6));
    EXPECT_EQ(6, lsm.countLess(7));
    EXPECT_EQ(6, lsm.countLess(8));
    EXPECT_EQ(6, lsm.countLess(9));
    EXPECT_EQ(7, lsm.countLess(10));
    lsm.push(7);
    {
        auto a = lsm.getCopy();
        EXPECT_EQ(8, a.size());
        EXPECT_EQ(1, a[0]);
        EXPECT_EQ(2, a[1]);
        EXPECT_EQ(3, a[2]);
        EXPECT_EQ(4, a[3]);
        EXPECT_EQ(4, a[4]);
        EXPECT_EQ(6, a[5]);
        EXPECT_EQ(7, a[6]);
        EXPECT_EQ(9, a[7]);
        EXPECT_EQ("LSM: 1 2 3 4 4 6 7 9 ", lsm.toString());
    }
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
