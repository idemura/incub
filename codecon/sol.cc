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

const string kBigIntZero(1, 0);

string bigint_prep(string s, int k) {
  reverse(s.begin(), s.end());
  for (auto &d : s) { d += k; }
  return move(s);
}

string bigint_from_string(string s) {
  return bigint_prep(move(s), -'0');
}
string bigint_from_int(int n) {
  return bigint_from_string(to_string(n));
}
string bigint_from_int(i64 n) {
  return bigint_from_string(to_string(n));
}

string bigint_string(string b) {
  return bigint_prep(move(b), +'0');
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
  }
  if (r.back() == 0 && r.size() > 1) r.pop_back();
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

string bigint_div2(Substr s) {
  string r(s.size(), 0);
  for (int i = 0; i < s.size(); i++) {
    if (s[i] & 1 && i > 0) {
      r[i - 1] += 5;
    }
    r[i] = s[i] >> 1;
  }
  if (r.back() == 0 && r.size() > 1) r.pop_back();
  return r;
}

void test_mult_scalar() {
  auto check_op = [](string a, int b) {
    return bigint_mult_scalar(bigint_from_string(a), b) ==
           bigint_from_int(stoi(a) * b);
  };
  CHECK(check_op("2", 3));
  CHECK(check_op("3", 7));
  CHECK(check_op("21", 3));
  CHECK(check_op("46", 3));
  CHECK(check_op("46", 5));
  CHECK(check_op("73", 8));
  CHECK(check_op("73", 0));
}

void test_sum() {
  auto check_op = [](string a, string b) {
    return bigint_sum(bigint_from_string(a), bigint_from_string(b)) ==
           bigint_from_int(stoi(a) + stoi(b));
  };
  CHECK(check_op("2", "3"));
  CHECK(check_op("9", "0"));
  CHECK(check_op("09", "1"));
  CHECK(check_op("09", "10"));
  CHECK(check_op("12", "13"));
  CHECK(check_op("17", "24"));
  CHECK(check_op("999", "4"));
  CHECK(check_op("27", "185"));
}

int pow10(int n) {
  return pow(10, n);
}

void test_sum_shift() {
  auto check_op = [](string a, int a_shift, string b) {
    return bigint_sum(bigint_from_string(a), bigint_from_string(b), a_shift) ==
           bigint_from_int(stoi(a) * pow10(a_shift) + stoi(b));
  };
  CHECK(check_op("2", 1, "3"));
  CHECK(check_op("2", 1, "32"));
  CHECK(check_op("9", 1, "13"));
  CHECK(check_op("9", 2, "13"));
  CHECK(check_op("1", 2, "7"));
  CHECK(check_op("20", 1, "0"));
  CHECK(check_op("0", 2, "12"));
}

void test_sub() {
  auto check_op = [](string a, string b) {
    return bigint_sub(bigint_from_string(a), bigint_from_string(b)) ==
           bigint_from_int(stoi(a) - stoi(b));
  };
  CHECK(check_op("3569", "1234"));
  CHECK(check_op("1569", "234"));
  CHECK(check_op("1561", "129"));
  CHECK(check_op("1569", "189"));
  CHECK(check_op("2000", "189"));
  CHECK(check_op("2000", "9"));
  CHECK(check_op("2000", "0"));
  CHECK(check_op("1000", "17"));
  CHECK(check_op("1000", "10"));
  CHECK(check_op("10", "10"));
  CHECK(check_op("6", "6"));
}

void test_mult() {
  auto check_op = [](string a, string b) {
    return bigint_mult(bigint_from_string(a), bigint_from_string(b)) ==
           bigint_from_int(stoi(a) * stoi(b));
  };
  CHECK(check_op("2", "3"));
  CHECK(check_op("12", "13"));
  CHECK(check_op("40", "35"));
  CHECK(check_op("123", "76"));
  CHECK(check_op("76", "123"));
  CHECK(check_op("76", "10001"));
  CHECK(check_op("276", "1001"));
  CHECK(check_op("276", "9999"));
  CHECK(check_op("276", "100"));
  CHECK(check_op("276", "0"));
  CHECK(check_op("5523", "1309"));
  CHECK(check_op("1937", "118"));
  CHECK(check_op("100", "100"));
}

void test_div2() {
  auto check_op = [](string a) {
    return bigint_div2(bigint_from_string(a)) == bigint_from_int(stoi(a) / 2);
  };
  CHECK(check_op("0"));
  CHECK(check_op("1"));
  CHECK(check_op("2"));
  CHECK(check_op("3"));
  CHECK(check_op("10"));
  CHECK(check_op("15"));
  CHECK(check_op("16"));
  CHECK(check_op("128"));
  CHECK(check_op("112"));
  CHECK(check_op("252"));
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  test_mult_scalar();
  test_sum();
  test_sum_shift();
  test_sub();
  test_mult();
  test_div2();
  cout << "TESTS PASSED." << endl;
  return 0;
}
