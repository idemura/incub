#include <bits/stdc++.h>

using namespace std;

// Every operation modifies a row. There are could be up to 100'000 rows.
// Represent matrix as vector of row index.

struct MxRowIds {
    int id[1001];
};

constexpr int MAX = 100001;
bitset<1001> row[MAX];
MxRowIds history[MAX];
int counter[MAX];
int n, m;

int main() {
    int q_num;
    scanf("%d%d%d", &n, &m, &q_num);
    for (int q = 1; q <= q_num; q++) {
        int op, i, j;
        scanf("%d%d", &op, &i);
        if (1 <= op && op <= 3) {
            history[q] = history[q - 1];
            // Imporant to copy row before we update id on the next line.
            row[q] = row[history[q].id[i]];
            history[q].id[i] = q;
            counter[q] = counter[q - 1];
            switch (op) {
            case 1:
                scanf("%d", &j);
                if (row[q][j] == 0) {
                    row[q][j] = 1;
                    counter[q]++;
                }
                break;
            case 2:
                scanf("%d", &j);
                if (row[q][j] == 1) {
                    row[q][j] = 0;
                    counter[q]--;
                }
                break;
            case 3: {
                int c = 0;
                for (int k = 1; k <= m; k++) {
                    c += row[q][k];
                    row[q][k] = !row[q][k];
                }
                counter[q] += m - 2 * c;
                break;
            }
            }
        } else {
            history[q] = history[i];
            row[q] = row[i];
            counter[q] = counter[i];
        }
        printf("%d\n", counter[q]);
    }
    return 0;
}
