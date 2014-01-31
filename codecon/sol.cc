#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <utility>
#include <math.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

struct Name {
  char s[12];
};

struct Edge {
  int c;  // City index in adjacency matrix.
  int w;  // Edge weight.
  Edge(): c(), w() {}
};

int cities_n;
Name *names;
Edge **edges;
int *edge_n;

int
void readAndSolve()
{
  scanf("%d", &cities_n);
  names = new Name[cities_n]();
  edges = new Edge*[cities_n]();
  edge_num = new int[cities_n]();
  for (int cn = 0; cn < cities_n; cn++) {
    scanf("%s%d", names[i].s, &edge_n[cn]);
    for (int i = 0; i < edge_n[cn]; i++) {
      scanf("%d%d", &edges[i].c, &edges[i].w);
      edges[i].c--;  // 1-base in input.
    }
  }

  int pairs;
  scanf("%d", &pairs);
  for (int i = 0; i< pairs; i++) {
    char s_name[12], d_name[12];
    scanf("%s%s", s_name, d_name);

  }
  for (int i = 0; i < cities_n; i++) {
    delete[] edges[i];
  }
  delete[] edges;
  delete[] names;
}

int main(int argc, char **argv)
{
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  int t = 0;
  scanf("%d", &t);
  while (t-- > 0) {
    readAndSolve();
  }
  return 0;
}
