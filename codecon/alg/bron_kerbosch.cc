#include "base.h"

class Set {
public:
    Set() = default;

    Set add(int i) {
        return Set(bm_ | (1u << i));
    }
    Set rem(int i) {
        return Set(bm_ & ~(1u << i));
    }
    Set isect(Set other) const {
        return Set(bm_ & other.bm_);
    }
    Set unite(Set other) const {
        return Set(bm_ | other.bm_);
    }
    bool in(int i) const {
        return (bm_ & (1u << i)) != 0;
    }
    bool empty() const {
        return bm_ == 0;
    }
    void print(ostream &os) const {
        os << "{ ";
        for (int i = 0; i < 30; i++) {
            if (in(i)) {
                os << i + 1 << " ";
            }
        }
        os << "}";
    }

private:
    explicit Set(unsigned int bm): bm_(bm) {}
    unsigned int bm_ = 0;
};

ostream &operator<<(ostream &os, Set s) {
    s.print(os);
    return os;
}

struct Tab {
    explicit Tab(int t): t(t) {}
    int t = 0;
};

ostream &operator<<(ostream &os, Tab t) {
    for (int i = 0; i < t.t; i++) {
        cout << "  ";
    }
    return os;
}

void bron_kerbosch_rec(int d, const vector<Set> &am, Set r, Set c, Set x) {
    // cout<<Tab(d)<<"bronKolchRec:\n";
    // cout<<Tab(d)<<"c: "<<c<<"\n";
    // cout<<Tab(d)<<"r: "<<r<<"\n";
    // cout<<Tab(d)<<"x: "<<x<<"\n";
    if (c.empty()) {
        if (x.empty()) {
            cout << r << "\n";
        }
        // cout<<Tab(d)<<"no more candidates\n";
        return;
    }
    // Actually, go through `c`s elements. No pivoting.
    for (int i = 0; i < am.size(); i++) {
        if (!c.in(i)) continue;
        // cout<<Tab(d)<<"vertex "<<i + 1<<"\n";
        bron_kerbosch_rec(d + 1, am, r.add(i), c.isect(am[i]), x.isect(am[i]));
        x = x.add(i);
        c = c.rem(i);
        // cout<<Tab(d)<<"new x: "<<x<<"\n";
        // cout<<Tab(d)<<"new c: "<<c<<"\n";
    }
    // cout<<Tab(d)<<"end\n";
}

void bron_kerbosch(const vector<Set> &am) {
    Set c;
    for (int i = 0; i < am.size(); i++) {
        c = c.add(i);
    }
    bron_kerbosch_rec(0, am, Set(), c, Set());
}

void add_edge(vector<Set> &am, int a, int b) {
    a--;
    b--; // Zero based.
    am[a] = am[a].add(b);
    am[b] = am[b].add(a);
}

int main() {
    vector<Set> am(6);
    add_edge(am, 1, 2);
    add_edge(am, 1, 5);
    add_edge(am, 2, 3);
    add_edge(am, 2, 5);
    add_edge(am, 3, 4);
    add_edge(am, 4, 5);
    add_edge(am, 4, 6);
    bron_kerbosch(am);
    return 0;
}
