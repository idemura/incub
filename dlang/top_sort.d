import std.algorithm, std.conv, std.stdio;

int[] topSort(int[][] al) {
  auto res = new int[](0);
  auto m = new int[](al.length);

  void dfs(int v) {
    if (m[v]) {
      if (m[v] == 1) {
        throw new Exception("Cycle detected");
      }
      return;
    }
    m[v] = 1;  // To detect a cycle.
    foreach (u; al[v]) {
      dfs(u);
    }
    res ~= v;
    m[v] = 2;  // Visited.
  }

  foreach (i, ref mi; m) {
    if (!mi) {
      dfs(cast(int)i);
      mi = 1;
    }
  }

  reverse(res);
  return res;
}

size_t indexOf(T)(T[] a, T x) {
  foreach (i, ai; a) {
    if (ai == x) return i;
  }
  throw new Exception("Not found");
}

bool checkTopSort(int[][] al, int[] ts) {
  foreach (i; 0..al.length) {
    foreach (j; 0..al[i].length) {
      auto ni = indexOf(ts, cast(int)i);
      auto nj = indexOf(ts, al[i][j]);
      if (nj < ni) {
        writefln("Top sort property broken at %s and %s", i, al[i][j]);
        writefln("Array:");
        writefln("  %s", ts);
        return false;
      }
    }
  }
  return true;
}

void main(string[] args) {
  // 0--1-----2--3
  //  \      /
  //   -4--5-
  auto al = new int[][](6, 0);
  al[0] = [1, 4];
  al[1] = [2];
  al[2] = [3];
  al[3] = [];
  al[4] = [5];
  al[5] = [2];
  try {
    auto ts = topSort(al);
    assert(checkTopSort(al, ts));
    writefln("%s", ts);
  } catch (Exception e) {
    writefln("Exception: %s", e.msg);
  }
}
