/*
A tree (i.e. a connected graph without cycles) with vertices is given
(N ≥ 2). Vertices of the tree are numbered by the integers 1,...,N. A Prufer
code for the tree is built as follows: a leaf (a vertex that is incident to the
only edge) with a minimal number is taken. Then this vertex and the incident
edge are removed from the graph, and the number of the vertex that was adjacent
to the leaf is written down. In the obtained graph once again a leaf with a
minimal number is taken, removed and this procedure is repeated until the only
vertex is left. It is clear that the only vertex left is the vertex with the
number N. The written down set of integers (N−1 numbers, each in a range from 1
to N) is called a Prufer code of the graph.
Your task is, given a Prufer code, to reconstruct a tree, i.e. to find out the
adjacency lists for every vertex in the graph.
You may assume that 2 ≤ N ≤ 7500

Input
A set of numbers corresponding to a Prufer code of some tree. The numbers are
separated with a spaces and/or line breaks.

Output
Adjacency lists for each vertex. Format: a vertex number, colon, numbers of
adjacent vertices separated with a space. The vertices inside lists and lists
itself should be sorted by vertex number in an ascending order (look at sample
output).
**/

#include <algorithm>
#include <vector>
#include <set>
#include <utility>
#include <stdio.h>
#include <string.h>
#include <math.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff
#define MOD 1000000007

using namespace std;

#define DIM 7500

int vs[DIM], vs_n = 0;
int cs[DIM], cs_n = 0;
vector<int> al[DIM];

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    set<int> heap;  // Seems set is good enough to be a heap.
    int v, i, j;

    while (scanf("%d", &v) == 1) {
        vs[vs_n++] = v - 1;
        if (v > cs_n) {
            cs_n = v;
        }
    }
    for (i = 0; i < vs_n; i++) {
        cs[vs[i]]++;
    }
    for (i = 0; i < cs_n; i++) {
        if (cs[i] == 0) {
            heap.insert(i);
        }
    }
    for (i = 0; i < vs_n; i++) {
        int v = *heap.begin();
        heap.erase(heap.begin());
        int w = vs[i];
        al[v].push_back(w);
        al[w].push_back(v);
        cs[w]--;
        if (cs[w] == 0) {
            heap.insert(w);
        }
    }
    for (i = 0; i < cs_n; i++) {
        sort(al[i].begin(), al[i].end());
        printf("%d:", i + 1);
        for (j = 0; j < al[i].size(); j++) {
            printf(" %d", al[i][j] + 1);
        }
        printf("\n");
    }
    return 0;
}
