#include "base.h"

using vector_int = vector<int>;

void print_mapping(const vector_int &b_to_a) {
    cout << "Mapping:" << endl;
    for (int i = 0; i < b_to_a.size(); i++) {
        cout << i << " - " << b_to_a[i] << endl;
    }
}

int get_projection_size(const vector<vector_int> &a) {
    int n = -1;
    for (int i = 0; i < a.size(); i++) {
        for (int j = 0; j < a[i].size(); j++) {
            if (a[i][j] > n) n = a[i][j];
        }
    }
    return n + 1;
}

bool dfs_bipart(
        const vector<vector_int> &a,
        int v, // Vertex in a.
        vector_int &m, // Marks on vertices of a.
        vector_int &b_to_a) {
    if (m[v]) return false;
    m[v] = 1;

    for (int i = 0; i < a[v].size(); i++) {
        int bi = a[v][i];
        if (b_to_a[bi] < 0 || dfs_bipart(a, b_to_a[bi], m, b_to_a)) {
            b_to_a[bi] = v;
            return true;
        }
    }
    return false;
}

void kuhn(const vector<vector_int> &a, int b_size, vector_int &b_to_a) {
    b_to_a.resize(b_size, -1);
    for (int i = 0; i < a.size(); i++) {
        vector_int m(a.size());
        dfs_bipart(a, i, m, b_to_a);
    }
}

void test(const vector<vector_int> &a) {
    vector_int b_to_a;
    kuhn(a, get_projection_size(a), b_to_a);
    print_mapping(b_to_a);
}

int main(int argc, char **argv) {
    {
        vector<vector_int> a(4);
        a[0].push_back(0);
        a[0].push_back(1);
        a[1].push_back(0);
        a[1].push_back(1);
        a[1].push_back(2);
        a[2].push_back(1);
        a[3].push_back(1);
        a[3].push_back(2);
        test(a);
    }
    {
        vector<vector_int> a(4);
        a[0].push_back(0);
        a[0].push_back(1);
        a[1].push_back(1);
        a[1].push_back(0);
        a[1].push_back(2);
        a[2].push_back(1);
        a[3].push_back(1);
        a[3].push_back(2);
        a[3].push_back(3);
        test(a);
    }
    {
        vector<vector_int> a(4);
        a[0].push_back(0);
        a[0].push_back(1);
        a[1].push_back(1);
        a[1].push_back(0);
        a[1].push_back(2);
        a[2].push_back(1);
        a[3].push_back(1);
        a[3].push_back(2);
        a[3].push_back(3);
        a[3].push_back(4);
        test(a);
    }
    return 0;
}
