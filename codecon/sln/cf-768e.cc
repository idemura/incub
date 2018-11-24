#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

int main() {
    int n;
    cin >> n;
    // Single game with marked "no-moves": its state is (number, [made_steps]).
    // If made_moves grows as arithmetic progression (longest possible) so the
    // score, analyze with pen and paper: (5,[]) and (6,[]).
    vector<int> ap(20);
    for (int i = 1; i < ap.size(); i++) {
        ap[i] = ap[i - 1] + i;
    }
    vector<int> ap_count;
    int c = 0;
    int a = 1;
    ap_count.push_back(0);
    for (int i = 1; i < ap.size(); i++) {
        for (; a < ap[i]; a++) {
            ap_count.push_back(c);
        }
        c++;
    }
    // Use Sprague-Grundy theorem:
    int sg_val = 0;
    for (int i = 0; i < n; i++) {
        int stones;
        cin >> stones;
        sg_val ^= ap_count[stones];
    }
    cout << (sg_val != 0 ? "NO" : "YES") << endl;
    return 0;
}
