#include "base.h"

// Read-only, non-owning, copyable substring of an existing string.
class Substr {
public:
  Substr() : s_(), l_() {}
  Substr(const char *s, size_t l) : s_(s), l_(l) {}
  Substr(const string &s) : Substr(s.data(), s.size()) {}
  char operator[](size_t i) const { return s_[i]; }
  char const* data() const { return s_; }
  size_t size() const { return l_; }
  void swap(Substr &other) {
    std::swap(s_, other.s_);
    std::swap(l_, other.l_);
  }
  Substr substr(size_t first, size_t last) const {
    return Substr(s_ + first, last - first);
  }
  string str() const { return string(s_, l_); }
private:
  char const *s_;
  size_t l_;
};

ostream& operator<<(ostream &os, Substr s) {
  return os.write(s.data(), s.size());
}

const string kBigIntZero("\0", 1);

string bigint_prep(string s, int k) {
  reverse(s.begin(), s.end());
  for (auto &d : s) { d += k; }
  return move(s);
}

string bigint_from_string(string s) {
  return bigint_prep(s, -'0');
}

string bigint_string(string b) {
  return bigint_prep(b, +'0');
}
string bigint_string(Substr b) {
  return bigint_string(b.str());
}

bool bigint_zero(Substr s) {
  return s.size() == 0 || (s.size() == 1 && s[0] == 0);
}

string bigint_mult_scalar(Substr s, int n) {
  if (n == 0) return kBigIntZero;
  string r(s.data(), s.size());
  int c = 0;
  for (auto &d : r) {
    int m = d * n + c;
    d = m % 10;
    c = m / 10;
  }
  if (c != 0) r.push_back(c);
  return r;
}

string bigint_sum(Substr a, Substr b, size_t a_shift = 0) {
  if (bigint_zero(a)) return b.str();
  if (bigint_zero(b)) {
    return a_shift == 0 ? a.str() : string(a_shift, 0) + a.str();
  }
  if (a.size() + a_shift < b.size()) a.swap(b);
  int min_s = min(a.size() + a_shift, b.size());
  int max_s = max(a.size() + a_shift, b.size());
  string r(max_s, 0);
  int i = 0, ai = 0, bi = 0;
  for (; i < min(a_shift, b.size()); i++) {
    r[i] = b[bi++];
  }
  // `r[a_shift .. min_s]` are already filled with 0.
  int c = 0;
  for (i = a_shift; i < min_s; i++) {
    r[i] = a[ai++] + b[bi++] + c;
    if (r[i] >= 10) {
      r[i] -= 10;
      c = 1;
    } else {
      c = 0;
    }
  }
  for (; i < max_s; i++) {
    r[i] = a[ai++];
  }
  if (c != 0) {
    for (int i = min_s; i < max_s && c != 0; i++) {
      if (r[i] == 9) {
        r[i] = 0;
      } else {
        r[i]++;
        c = 0;
      }
    }
    if (c != 0) r.push_back(c);
  }
  return r;
}

// Assumes `a >= b`.
string bigint_sub(Substr a, Substr b) {
  if (bigint_zero(b)) return a.str();
  int min_s = min(a.size(), b.size());
  string r(a.data(), a.size());
  int c = 0;
  for (int i = 0; i < min_s; i++) {
    if (c != 0) {
      if (r[i] == 0) {
        r[i] = 9 - b[i];
        continue;
      } else {
        r[i]--;
        c = 0;
      }
    }
    if (r[i] < b[i]) {
      c = 1;
      r[i] += 10 - b[i];
    } else {
      r[i] -= b[i];
    }
  }
  if (c != 0) {
    int i = min_s;
    while (r[i] == 0) {
      r[i] = 9;
      i++;
    }
    r[i]--;
    if (r.back() == 0) r.pop_back();
  }
  return r;
}

// Karatsuba algorithm.
string bigint_mult(Substr a, Substr b) {
  if (bigint_zero(a) || bigint_zero(b)) return kBigIntZero;
  if (a.size() < b.size()) a.swap(b);
  if (b.size() == 1) {
    return bigint_mult_scalar(a, b[0]);
  }
  int m = a.size() / 2;
  auto al = a.substr(0, m);
  auto ah = a.substr(m, a.size());
  auto bl = b.substr(0, min<int>(m, b.size()));
  auto bh = m > b.size() ? Substr(kBigIntZero) : b.substr(m, b.size());
  auto s = bigint_mult(bigint_sum(al, ah),
                       bigint_sum(bl, bh));
  auto hh = bigint_mult(ah, bh);
  auto ll = bigint_mult(al, bl);
  auto lh = bigint_sub(s, bigint_sum(ll, hh));
  return bigint_sum(hh, bigint_sum(lh, ll, m), 2 * m);
}

void test_mult_scalar() {
  auto check_op = [](string b, int n, string r) {
    return bigint_string(bigint_mult_scalar(bigint_from_string(b), n)) == r;
  };
  CHECK(check_op("2", 3, "6"));
  CHECK(check_op("3", 7, "21"));
  CHECK(check_op("21", 3, "63"));
  CHECK(check_op("46", 3, "138"));
  CHECK(check_op("46", 5, "230"));
  CHECK(check_op("73", 8, "584"));
  CHECK(check_op("73", 0, "0"));
}

void test_sum() {
  auto check_op = [](string a, string b, string r) {
    return bigint_string(
        bigint_sum(bigint_from_string(a),
                   bigint_from_string(b))) == r;
  };
  CHECK(check_op("2", "3", "5"));
  CHECK(check_op("9", "0", "9"));
  CHECK(check_op("9", "", "9"));
  CHECK(check_op("09", "1", "10"));
  CHECK(check_op("09", "10", "19"));
  CHECK(check_op("12", "13", "25"));
  CHECK(check_op("17", "24", "41"));
  CHECK(check_op("999", "4", "1003"));
  CHECK(check_op("27", "185", "212"));
}

void test_sum_shift() {
  auto check_op = [](string a, int a_shift, string b, string r) {
    return bigint_string(
        bigint_sum(bigint_from_string(a),
                   bigint_from_string(b), a_shift)) == r;
  };
  CHECK(check_op("2", 1, "3", "23"));
  CHECK(check_op("2", 1, "32", "52"));
  CHECK(check_op("9", 1, "13", "103"));
  CHECK(check_op("9", 2, "13", "913"));
  CHECK(check_op("1", 2, "7", "107"));
  CHECK(check_op("20", 1, "0", "200"));
  CHECK(check_op("0", 2, "12", "12"));
}

void test_sub() {
  auto check_op = [](string a, string b, string r) {
    return bigint_string(
        bigint_sub(bigint_from_string(a),
                   bigint_from_string(b))) == r;
  };
  CHECK(check_op("3569", "1234", "2335"));
  CHECK(check_op("1569", "234", "1335"));
  CHECK(check_op("1561", "129", "1432"));
  CHECK(check_op("1569", "189", "1380"));
  CHECK(check_op("2000", "189", "1811"));
  CHECK(check_op("2000", "9", "1991"));
  CHECK(check_op("2000", "0", "2000"));
  CHECK(check_op("1000", "17", "983"));
  CHECK(check_op("1000", "10", "990"));
}

void test_mult() {
  auto check_op = [](string a, string b, string r) {
    return bigint_string(
        bigint_mult(bigint_from_string(a),
                    bigint_from_string(b))) == r;
  };
  CHECK(check_op("2", "3", "6"));
  CHECK(check_op("12", "13", "156"));
  CHECK(check_op("40", "35", "1400"));
  CHECK(check_op("123", "76", "9348"));
  CHECK(check_op("76", "123", "9348"));
  CHECK(check_op("76", "10001", "760076"));
  CHECK(check_op("276", "1001", "276276"));
  CHECK(check_op("276", "9999", "2759724"));
  CHECK(check_op("276", "100", "27600"));
  CHECK(check_op("276", "0", "0"));
  CHECK(check_op("5523", "1309", "7229607"));
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  test_mult_scalar();
  test_sum();
  test_sum_shift();
  test_sub();
  test_mult();
  cout << "TESTS PASSED." << endl;
  return 0;
}
