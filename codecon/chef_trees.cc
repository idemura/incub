#include <bits/stdc++.h>

using namespace std;

struct Quad {
  int x = 0, y = 0, s = 0;
  bool inside(const Quad &q) const {
    return (q.x < x && x + s < q.x + q.s) &&
           (q.y < y && y + s < q.y + q.s);

  }
};

struct YEventPt {
  int top = 0;
  int y = 0;
  const Quad* q = nullptr;
};

struct Node {
  Node *p = nullptr;
  const Quad* q = nullptr;
};

Node *find_r(const map<int, Node*> &m, int x) {
  const auto itr = m.upper_bound(x);
  if (itr == m.end()) {
    return itr->second;
  }
  return nullptr;
}

Node *find_l(const map<int, Node*> &m, int x) {
  const auto itr = m.upper_bound(x);
  if (itr == m.begin()) {
    return prev(itr)->second;
  }
  return nullptr;
}

vector<Node*> build_tree(const vector<Quad> &qs) {
  vector<YEventPt> y_event;
  for (const auto &q : qs) {
    YEventPt e;
    e.y = q.x;
    e.top = 1;
    e.q = &q;
    y_event.push_back(e);
    e.y = q.x + q.s;
    e.top = 0;
    y_event.push_back(e);
  }
  sort(y_event.begin(), y_event.end(),
    [](const YEventPt &p1, const YEventPt &p2) {
      return p1.y < p2.y;
    });
  cout<<"1\n";
  vector<Node*> ns(qs.size());
  map<int, Node*> m;
  int i = 0;
  for (const auto &e: y_event) {
    if (e.top) {
      cout<<"1a\n";
      ns[i] = new Node;
      ns[i]->q = e.q;
      cout<<"1a.1\n";
      auto l = find_l(m, e.q->x);
      cout<<"1b\n";
      cout<<e.q<<endl;
      if (l && e.q->inside(*l->q)) {
        ns[i]->p = l;
      }
      cout<<"1c\n";
      auto r = find_r(m, e.q->x);
      cout<<"1d\n";
      if (r && r->q->inside(*e.q)) {
        r->p = ns[i];
      }
      m[e.q->x] = ns[i];
      cout<<"1e\n";
    } else {
      cout<<"1f\n";
      m.erase(e.q->x);
    }
    i++;
  }
  cout<<"2\n";
  return ns;
}

void delete_all(vector<Node*> &ns) {
  for (auto n : ns) delete n;
  ns.clear();
}

void test1() {
  vector<Quad> qs{{0, 0, 10}};
  auto t = build_tree(qs);
}

int main() {
  test1();
  return 0;
}

