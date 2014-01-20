#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

struct Name {
  std::string s;
  int length, vowels;

  Name()
    : length(), vowels() {
  }
};

double **M;
int Mn;

bool isVowel(int c)
{
  return strchr("aeiouAEIOU", (char)c) != NULL;
}

bool isAlpha(int c) {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
}

int gcd(int a, int b)
{
    while (b != 0) {
        int t = a % b;
        a = b;
        b = t;
    }
    return a;
}

double value(const Name &cons, const Name &prod)
{
  double v;
  if (prod.length & 1) {
    v = cons.length - cons.vowels;
  } else {
    v = cons.vowels * 1.5;
  }
  if (gcd(cons.length, prod.length) > 1) {
    v *= 1.5;
  }
  return v;
}

double permute()
{
  printf("Mn %d\n");
  std::vector<int> idx(Mn);
  for (int i = 0; i < Mn; i++) {
    idx[i] = i;
  }
  double max_v = -1;
  do {
    double v = 0;
    for (int i = 0; i < Mn; i++) {
      v += M[i][idx[i]];
    }
    printf("%.2lf\n", v);
    if (v > max_v) {
      max_v = v;
    }
  } while (std::next_permutation(idx.begin(), idx.end()));
  printf("return %.2lf\n", max_v);
  return max_v;
}

void readNames(FILE *f, int sep, std::vector<Name> *out)
{
  int chr = 0;
  Name nm;
  while (!feof(f) && (chr = fgetc(f)) != sep) {
    if (chr == ',') {
      out->push_back(nm);
      nm.s.clear();
      nm.length = nm.vowels = 0;
    } else {
      nm.s.push_back(chr);
      if (isAlpha(chr)) {
        nm.length++;
        if (isVowel(chr)) {
          nm.vowels++;
        }
      }
    }
  }
  if (nm.length) {
    out->push_back(nm);
  }
}

double getMaxValue(FILE *f)
{
  std::vector<Name> cs, ps;
  readNames(f, ';', &cs);
  readNames(f, '\n', &ps);
  if (cs.size() != ps.size() || cs.empty()) {
    printf("whooops! %d %d\n", cs.size(), ps.size());
    return -1;
  }

  Mn = cs.size();
  M = new double*[Mn];
  M[0] = new double[Mn * Mn];
  for (int i = 1; i < Mn; i++) {
    M[i] = M[0] + i * Mn;
  }
  for (int i = 0; i < Mn; i++) {
    printf("Consumer %s\n", cs[i].s.c_str());
    for (int j = 0; j < Mn; j++) {
      M[i][j] = value(cs[i], ps[j]);
      printf("  Prod %s = %.2lf\n", ps[j].s.c_str(), M[i][j]);
    }
  }

  double max_v = permute();

  delete[] M[0];
  delete[] M;

  return max_v;
}

int main(int argc, char **argv)
{
  const char *in_name = argv[1];
  FILE *f = fopen(in_name, "rt");
  if (f) {
    while (!feof(f)) {
      double max_v = getMaxValue(f);
      // if (max_v > 0) {
      {
        printf("%.2lf\n", max_v);
      }
    }
    fclose(f);
  }
  return 0;
}
