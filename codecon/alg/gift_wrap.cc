#include "base.h"

// Convex hull: Gift wrapping algorithm.

struct Pt2D {
    int x = 0, y = 0;
    Pt2D() {}
    Pt2D(int x, int y): x(x), y(y) {}
};

ostream &operator<<(ostream &os, Pt2D p) {
    return os << "x=" << p.x << ",y=" << p.y;
}

Pt2D operator-(Pt2D a, Pt2D b) {
    a.x -= b.x;
    a.y -= b.y;
    return a;
}

Pt2D operator+(Pt2D a, Pt2D b) {
    a.x += b.x;
    a.y += b.y;
    return a;
}

// Return true if c is to the right of the CW line a->b.
bool cw_right(Pt2D a, Pt2D b, Pt2D c) {
    b = b - a;
    c = c - a;
    return i64(b.x) * c.y - i64(b.y) * c.x < 0;
}

vector<Pt2D> convex(vector<Pt2D> a) {
    if (a.size() <= 3) {
        return a;
    }
    auto pivot = *min_element(a.begin(), a.end(), [](Pt2D a, Pt2D b) {
        if (a.x == b.x)
            return a.y < b.y;
        else
            return a.x < b.x;
    });
    for (auto &p : a) {
        p = p - pivot;
    }
    sort(a.begin(), a.end(), [](Pt2D a, Pt2D b) {
        if (a.x == 0 && b.x == 0) {
            return a.y < b.y;
        } else {
            return i64(a.y) * b.x > i64(a.x) * b.y;
        }
    });
    vector<Pt2D> c;
    c.push_back(a[0]);
    c.push_back(a[1]);
    for (int i = 2; i < a.size();) {
        auto b = cw_right(c[c.size() - 2], c[c.size() - 1], a[i]);
        if (!b) {
            // Do not advance, while while point is counter clock wise.
            c.pop_back();
            b = c.size() == 1;
        }
        if (b) {
            c.push_back(a[i]);
            i++;
        }
    }
    for (auto &p : c) {
        p = p + pivot;
    }
    return c;
}

void test(const vector<Pt2D> &a) {
    cout << "input:" << endl;
    for (auto p : a) {
        cout << "  " << p << endl;
    }
    auto c = convex(a);
    cout << "convex poly:" << endl;
    for (auto p : c) {
        cout << "  " << p << endl;
    }
}

int main() {
    vector<Pt2D> a;
    a.emplace_back(0, 1);
    a.emplace_back(0, 10);
    a.emplace_back(5, 10);
    a.emplace_back(5, -3);
    a.emplace_back(0, -3);
    a.emplace_back(0, -2);
    a.emplace_back(3, 3);
    test(a);
    return 0;
}
