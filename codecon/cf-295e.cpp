#include <iostream>
#include <vector>
#include <algorithm>
#include <cstdio>

using namespace std;

typedef long long ll;
const int MAX = int(1e5) + 10;

struct node{
    int cc;
    ll sum1, sum2;
} T[MAX << 3];

int N, M;
int a[MAX], cur[MAX];
int typ[MAX], x[MAX], y[MAX];
vector<int> X;

int getID(int x){
    return lower_bound(X.begin(), X.end(), x) - X.begin();
}

node merge(node &L, node &R){
    node ret;
    ret.cc = L.cc + R.cc;
    ret.sum1 = L.sum1 + R.sum1;
    ret.sum2 = L.sum2 + R.sum2 + L.cc * R.sum1;
    return ret;
}

void update(int nod, int lo, int hi, int p, int v, int c){
    if(p < lo || hi < p)    return;
    if(lo == p && hi == p){
        T[nod].cc += c;
        T[nod].sum1 += v;
        T[nod].sum2 = T[nod].cc * T[nod].sum1;
        return;
    }

    int mit = (lo + hi) >> 1, I = nod << 1, D = I + 1;

    update(I, lo, mit, p, v, c);
    update(D, mit + 1, hi, p, v, c);

    T[nod] = merge(T[I], T[D]);
}

node query(int nod, int lo, int hi, int a, int b){
    if(b < lo || hi < a)
        return (node){0, 0, 0};
    if(a <= lo && hi <= b)
        return T[nod];

    int mit = (lo + hi) >> 1, I = nod << 1, D = I + 1;

    node L = query(I, lo, mit, a, b);
    node R = query(D, mit + 1, hi, a, b);

    return merge(L, R);
}

int main() {
    int i;

    scanf("%d", &N);
    for(i = 0; i < N; i++){
        scanf("%d", a + i);
        cur[i] = a[i];
        X.push_back(a[i]);
    }

    int Q;
    scanf("%d", &Q);
    for(i = 0; i < Q; i++){
        scanf("%d%d%d", typ + i, x + i, y + i);
        if(typ[i] == 1){
            x[i]--;
            cur[x[i]] += y[i];
            X.push_back(cur[x[i]]);
        }
    }

    sort(X.begin(), X.end());
    X.erase(unique(X.begin(), X.end()), X.end());

    M = (int)X.size();
    for(i = 0; i < N; i++){
        update(1, 0, M - 1, getID(a[i]), a[i], 1);
        cur[i] = a[i];
    }

    for(i = 0; i < Q; i++){
        if(typ[i] == 1){
            update(1, 0, M - 1, getID(cur[x[i]]), -cur[x[i]], -1);
            cur[x[i]] += y[i];
            update(1, 0, M - 1, getID(cur[x[i]]), cur[x[i]], 1);
        }else{
            int L = lower_bound(X.begin(), X.end(), x[i]) - X.begin();
            int R = upper_bound(X.begin(), X.end(), y[i]) - X.begin();
            ll ans = 0;

            if(L < R){
                node seg = query(1, 0, M - 1, L, R - 1);
                ans = 2 * seg.sum2 - (seg.cc + 1) * seg.sum1;
            }

            printf("%I64d\n", ans);
        }
    }

    return 0;
}
