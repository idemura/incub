#include <algorithm>
#include <functional>
#include <list>
#include <map>
#include <memory>
#include <random>
#include <string>
#include <unordered_map>
#include <vector>
#include <sstream>
#include <utility>
#include <iostream>
#include <cmath>  // Overloads for abs.

using namespace std;

using i64 = long long int;
using u64 = unsigned long long int;
using i32 = int;
using u32 = unsigned int;

constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

int main(int argc, char **argv) {
  //ios_base::sync_with_stdio(false);
  int n = 0, k = 0;
  scanf("%d%d", &n, &k);
  printf("n=%d k=%d\n", n, k);
  vector<int> a(n);
  for (int i = 0; i < n; i++) {
    scanf("%d", &a[i]);
    printf("%d ", a[i]);
  }
  printf("\n");
  unordered_map<int, int> right_pos;
  int longest_l = 0, longest_r = 0;
  int l = 0;
  for (int i = 0; i < n; i++) {
    cout<<"--------"<<endl;
    cout<<"right map:"<<endl;
    for (auto p: right_pos) cout<<"key: "<<p.first<<" pos: "<<p.second<<endl;
    cout<<"left: "<<l<<" right: "<<i<<endl;
    cout<<"card: "<<right_pos.size()<<endl;
    cout<<"consider "<<a[i]<<" <<<"<<endl;

    auto it = right_pos.find(a[i]);
    if (right_pos.end() == it) {
      cout<<"not found in the map, cardinality increased"<<endl;
      // See a[i] first time (since @l). Increase cardinality.
      if (right_pos.size() == k) {
        cout<<"cardinality over limit!"<<endl;
        // Pull left end to right maintaining longest possible until cardinality
        // drops by one.
        cout<<"starting "<<l<<endl;
        for (; l < i; l++) {
          cout<<"l="<<l<<" a="<<a[l]<<" right pos: "<<right_pos[a[l]]<<endl;
          if (right_pos[a[l]] == l) {
            cout<<"erase it!"<<endl;
            right_pos.erase(a[l++]);
            break;
          }
        }
      }
      cout<<"new/updated in map, pos "<<i<<endl;
      right_pos[a[i]] = i;
    } else {
      cout<<"cardinality is not increased, updated right"<<endl;
      right_pos[a[i]] = i;
    }
    cout<<"l="<<l<<" i="<<i<<" longest_l="<<longest_l<<" longest_r="<<longest_r<<endl;
    if (i - l > longest_r - longest_l) {
      cout<<"update answer"<<endl;
      longest_l = l;
      longest_r = i;
    }
  }
  printf("%d %d\n", longest_l + 1, longest_r + 1);
  return 0;
}

