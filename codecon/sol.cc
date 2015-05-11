#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <unordered_map>
#include <string>
#include <queue>
#include <vector>
#include <memory>
#include <sstream>
#include <utility>
#include <math.h>
#include <stdlib.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C) \
    C(const C&) = delete; \
    C& operator=(const C&) = delete;
#define NEW_UNIQUE(T) unique_ptr<T>(new T)
#define CHECK(E) \
  do { \
      if (!(E)) { \
        cout << "CHECK failed at " << __FILE__ << "@" << __LINE__ << endl; \
        exit(EXIT_FAILURE); \
    } \
  } while (false)

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 100000007;

class PrinceOfPersia {
public:
  struct Pos {
    int x, y;
    Pos(): x(), y() {}
    Pos(int x, int y): x(x), y(y) {}
  };

  // BFS
  int minObstacles(vector<string> maze_in) {
    this->maze = maze_in;
    front.push_back(get_initial_pos());
    if (p_nearby(init.x, init.y)) {
      return -1;
    }
    cout<<"maze in"<<endl;
    print_maze();
    eliminate_dead_ends();
    cout<<"maze no dead ends:"<<endl;
    print_maze();
    vector<int> levels;
    for (int i = 0; i < front.size(); ) {
      level = 0;
      cout<<"next level!\n";
      for (int n = front.size(); i < n; i++) {
        cout<<"front "<<(front[i].x+1)<<" "<<(front[i].y+1)<<endl;
        print_maze();
        update_pos(front[i],  1,  0);
        update_pos(front[i], -1,  0);
        update_pos(front[i],  0,  1);
        update_pos(front[i],  0, -1);
      }
      cout<<"--- level="<<level<<endl;
      if (level > 0) levels.push_back(level);
    }
    if (!reached) return 0;
    return *min_element(levels.begin(), levels.end());
  }

  Pos get_initial_pos() {
    for (int i = 0; i < maze.size(); i++) {
      for (int j = 0; j < maze[i].size(); j++) {
        if (maze[i][j] == 'P') {
          init.x = i;
          init.y = j;
          return init;
        }
      }
    }
    return init;
  }

  bool update_pos(Pos p, int x, int y) {
    x += p.x;
    y += p.y;
    if (outside(x, y)) return false;
    if (maze[x][y] == '.') {
      maze[x][y] = '*';
      front.emplace_back(x, y);
      level++;
      return true;
    }
    if (maze[x][y] == 'P' && x != init.x && y != init.y) {
      reached = true;
      return true;
    }
    return false;
  }

  bool outside(int x, int y) {
    return x < 0 || x >= maze.size() || y < 0 || y >= maze[0].size();
  }

  bool p_nearby(int x, int y) {
    return (!outside(x + 1, y) && maze[x + 1][y] == 'P') ||
           (!outside(x - 1, y) && maze[x - 1][y] == 'P') ||
           (!outside(x, y + 1) && maze[x][y + 1] == 'P') ||
           (!outside(x, y - 1) && maze[x][y - 1] == 'P');
  }

  void eliminate_dead_ends() {
    int n_dead_ends = 0;
    do {
      n_dead_ends = 0;
      cout<<"----\n";
      print_maze();
      for (int i = 0; i < maze.size(); i++) {
        for (int j = 0; j < maze[i].size(); j++) {
          if (maze[i][j] != '.') continue;
          int n = (outside(i - 1, j) || maze[i - 1][j] == '#') +
                  (outside(i + 1, j) || maze[i + 1][j] == '#') +
                  (outside(i, j - 1) || maze[i][j - 1] == '#') +
                  (outside(i, j + 1) || maze[i][j + 1] == '#');
          cout<<"for x="<<(i + 1)<<" y="<<(j + 1)<<" n="<<n<<endl;
          if (n >= 3) {
            maze[i][j] = '#';
            n_dead_ends++;
          }
        }
      }
    } while (n_dead_ends > 0);
  }

  int length(int x, int y) { return abs(x - init.x) + abs(y - init.y); }

  void print_maze() {
    for (auto &s : maze) cout << s << endl;
  }

  vector<string> maze;
  Pos init;
  vector<Pos> front;
  int level = 0;
  bool reached = false;
};

int main() {
  cout << NEW_UNIQUE(PrinceOfPersia)->minObstacles(
      { "P....",
        "...##",
        "##...",
        "....P" }) << endl;
  cout << NEW_UNIQUE(PrinceOfPersia)->minObstacles(
    { ".....",
      ".P.P.",
      "....." }) << endl;
  return 0;
}
