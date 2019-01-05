#include "base.h"

// Min heap for value type of a non-exceptional type T.
template <class T, class C = std::less<T>>
class Heap {
public:
    explicit Heap(C c = C()): cmp(c) {}
    explicit Heap(std::vector<T> data, C c = C()): cmp(c), h(std::move(data)) {
        std::make_heap(h.begin(), h.end(), cmp);
    }

    void push(T v) {
        h.push_back(v);
        std::push_heap(h.begin(), h.end(), cmp);
    }

    bool empty() const {
        return h.empty();
    }
    
    T top() const {
        return h[0];
    }

    T pop() {
        std::pop_heap(h.begin(), h.end(), cmp);
        auto min = h.back();
        h.pop_back();
        return min;
    }

private:
    struct SwapCmp {
        explicit SwapCmp(C c): c(c) {}
        bool operator()(T a, T b) const {
            return c(b, a);
        }
        C c;
    } cmp;
    std::vector<T> h;
};

template <class C>
struct CmpFirst {
    explicit CmpFirst(C c): c(c) {}
    template <class P>
    bool operator()(const P &a, const P &b) const {
        return c(a.first, b.first);
    }
    C c;
};

// Min heap for value type of a non-exceptional type pair<K, V>.
template <class K, class V, class C = std::less<K>>
class HeapKeyVal: public Heap<std::pair<K, V>, CmpFirst<C>> {
public:
    using Pair = std::pair<K, V>;
    using Base = Heap<Pair, CmpFirst<C>>;
    // `push` overloads are hidden if no this line.
    using Base::push;
    explicit HeapKeyVal(C c = C()): Base(CmpFirst<C>(c)) {}
    explicit HeapKeyVal(std::vector<Pair> data, C c = C()):
            Base(data, CmpFirst<C>(c)) {}
    void push(K k, const V &v) {
        Base::push(make_pair(k, v));
    }
};

void test1() {
    const string ka = "a";
    const string kb = "b";
    const string kc = "c";
    HeapKeyVal<int, string> h;
    h.push(make_pair(12, ka));
    h.push(make_pair(11, kb));
    h.push(20, kc);
    CHECK(h.pop() == make_pair(11, kb));
    CHECK(h.pop() == make_pair(12, ka));
    CHECK(h.pop() == make_pair(20, kc));
}

template <class T, class Cmp = std::less<T>>
class ExtHeap {
public:
    typedef T ValueT;

    explicit ExtHeap(const Cmp &cmp = Cmp()): cmp(cmp) {}

    void insert(const T &val, size_t *index = NULL) {
        h.push_back(Elem(val, index));
        setIndex(h.size() - 1);
        heapifyParent(h.size() - 1);
        check();
    }

    // If you changed priority (either decrease or increase call this to
    // restore heap property.
    void reheapAt(size_t i) {
        if (i > 0 && !less((i - 1) / 2, 0)) {
            heapifyParent(i);
        } else {
            for (;;) {
                size_t kmin = i, k;
                k = 2 * i + 1;
                if (k < h.size() && less(k, kmin)) {
                    kmin = k;
                }
                k = 2 * i + 2;
                if (k < h.size() && less(k, kmin)) {
                    kmin = k;
                }
                if (kmin != i) {
                    std::swap(h[kmin], h[i]);
                    setIndex(i);
                    setIndex(kmin);
                    i = kmin;
                } else {
                    break;
                }
            }
        }
        // check();
    }

    T pop() {
        return remove(0);
    }

    T remove(size_t i) {
        // Swap i-th and last and restore heap property (heapify). If the last
        // element was less than i-th, restore up, else - restore down.
        CHECK(i < h.size());
        h[i].setIndex(kNoIndex);
        const T val = h[i].val;
        h[i] = h[h.size() - 1];
        setIndex(i);
        h.pop_back();
        if (h.size() > 1) {
            reheapAt(i);
        }
        return val;
    }

    size_t size() const {
        return h.size();
    }

private:
    struct Elem {
        T val;
        size_t *index;

        Elem(const T &val, size_t *index): val(val), index(index) {
            setIndex(kNoIndex);
        }

        void setIndex(size_t i) {
            if (index) *index = i;
        }
    };

    void setIndex(size_t i) {
        h[i].setIndex(i);
    }

    bool less(size_t i, size_t j) const {
        return cmp(h[i].val, h[j].val);
    }

    void heapifyParent(size_t i) {
        for (; i > 0;) {
            size_t p = (i - 1) / 2;
            if (less(p, i)) {
                break;
            }
            std::swap(h[p], h[i]);
            setIndex(p);
            setIndex(i);
            i = p;
        }
    }

    void check() const {
        for (size_t i = 1; i < h.size(); i++) {
            size_t p = (i - 1) / 2;
            if (less(i, p)) {
                std::cerr << "Heap corrupted at " << i << " (parent " << p
                          << ")" << std::endl;
            }
        }
    }

    static const size_t kNoIndex = -1;
    Cmp cmp;
    std::vector<Elem> h;
};

void testHeapRemove(const std::vector<int> &a, int rmi) {
    std::vector<size_t> ind(a.size());
    ExtHeap<int> heap;
    for (int i = 0; i < a.size(); i++) {
        heap.insert(a[i], &ind[i]);
    }
    heap.remove(ind[rmi]);
    // `pop` will produce a sorted sequence.
    auto prev = heap.pop();
    for (; heap.size() > 0;) {
        auto x = heap.pop();
        CHECK(prev <= x);
        prev = x;
    }
    std::cout << "Test case " << rmi << " OK." << std::endl;
}

int main(int argc, char **argv) {
    std::vector<int> a{9, 5, 1, 7, 8, 2};
    for (int i = 0; i < a.size(); i++) {
        testHeapRemove(a, i);
    }
    test1();
    cout << "TESTS PASSED." << endl;
    return 0;
}
