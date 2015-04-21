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
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C) \
    C(const C&) = delete; \
    C& operator=(const C&) = delete;

using namespace std;

using i64 = long long int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int DIM = 0;

class KeyValueStore {
public:
  KeyValueStore() {}
  void set(const string &key, const string &value);
  bool get(const string &key, string *value) const;
  void del(const string &key);
  int count(const string &value) const;
  void begin();
  bool rollback();
  bool commit();

private:
  void dec_value_counter(const string &value);
  bool has_transation() const { return !start_.empty(); }

  unordered_map<string, string> keys_;
  unordered_map<string, int> count_;

  vector<function<void()>> playback_;
  vector<int> start_;
};

void KeyValueStore::set(const string &key, const string &value) {
  auto i = keys_.find(key);
  if (i != keys_.end()) {
    dec_value_counter(i->second);
    if (has_transation()) {
      playback_.push_back(bind(&KeyValueStore::set, this, key, i->second));
    }
  } else {
    if (has_transation()) {
      playback_.push_back(bind(&KeyValueStore::del, this, key));
    }
  }
  keys_[key] = value;
  count_[value]++;
}

bool KeyValueStore::get(const string &key, string *value) const {
  auto i = keys_.find(key);
  if (i == keys_.end()) {
    value->clear();
    return false;
  }
  *value = i->second;
  return true;
}

void KeyValueStore::dec_value_counter(const string &value) {
  auto i = count_.find(value);
  if (i->second == 1) {
    count_.erase(i);
  } else {
    i->second--;
  }
}

void KeyValueStore::del(const string &key) {
  auto i = keys_.find(key);
  if (i == keys_.end()) {
    return;
  }
  dec_value_counter(i->second);
  keys_.erase(i);
  if (has_transation()) {
    playback_.push_back(bind(&KeyValueStore::set, this, key, i->second));
  }
}

int KeyValueStore::count(const string &value) const {
  auto i = count_.find(value);
  return i == count_.end() ? 0 : i->second;
}

void KeyValueStore::begin() {
  start_.push_back(playback_.size());
}

bool KeyValueStore::rollback() {
  if (!has_transation()) {
    return false;
  }
  auto n = start_.back();
  start_.pop_back();
  for (int j = playback_.size(); n < j; j--) {
    playback_[j - 1]();
  }
  playback_.resize(n);
  return true;
}

bool KeyValueStore::commit() {
  if (!has_transation()) {
    return false;
  }
  start_.clear();
  playback_.clear();
  return true;
}

#define CHECK(e) do { \
    if (!(e)) { \
      cout << "Test failed " << __FILE__ << ":" << __LINE__ << endl; \
      exit(-1); \
    } \
  } while (false)

void test1() {
  KeyValueStore kv;
  string s;
  kv.set("a", "10");
  CHECK(kv.get("a", &s) && s == "10");
  kv.del("a");
  CHECK(!kv.get("a", &s));
  cout << "Test 1 passed" << endl;
}

void test2() {
  KeyValueStore kv;
  string s;
  kv.set("a", "10");
  kv.set("b", "10");
  CHECK(kv.count("10") == 2);
  CHECK(kv.count("20") == 0);
  kv.del("a");
  CHECK(kv.count("10") == 1);
  kv.set("b", "30");
  CHECK(kv.count("10") == 0);
  cout << "Test 2 passed" << endl;
}

void test3() {
  KeyValueStore kv;
  string s;
  kv.begin();
  kv.set("a", "10");
  CHECK(kv.get("a", &s) && s == "10");
  kv.begin();
  kv.set("a", "20");
  CHECK(kv.get("a", &s) && s == "20");
  CHECK(kv.rollback());
  CHECK(kv.get("a", &s) && s == "10");
  CHECK(kv.rollback());
  CHECK(!kv.get("a", &s));
  cout << "Test 3 passed" << endl;
}

void test4() {
  KeyValueStore kv;
  string s;
  kv.begin();
  kv.set("a", "30");
  kv.begin();
  kv.set("a", "40");
  CHECK(kv.commit());
  CHECK(kv.get("a", &s) && s == "40");
  CHECK(!kv.rollback());
  cout << "Test 4 passed" << endl;
}

void test5() {
  KeyValueStore kv;
  string s;
  kv.set("a", "50");
  kv.begin();
  CHECK(kv.get("a", &s) && s == "50");
  kv.set("a", "60");
  kv.begin();
  kv.del("a");
  CHECK(!kv.get("a", &s));
  CHECK(kv.rollback());
  CHECK(kv.get("a", &s) && s == "60");
  CHECK(kv.commit());
  CHECK(kv.get("a", &s) && s == "60");
  cout << "Test 5 passed" << endl;
}

void test6() {
  KeyValueStore kv;
  string s;
  kv.set("a", "10");
  kv.begin();
  CHECK(kv.count("10") == 1);
  kv.begin();
  kv.del("a");
  CHECK(kv.count("10") == 0);
  CHECK(kv.rollback());
  CHECK(kv.count("10") == 1);
  cout << "Test 6 passed" << endl;
}

vector<string> split(const string &s) {
  vector<string> parts;
  const string whites(" \t\n\r");
  for (size_t i = 0;;) {
    auto j = s.find_first_of(whites, i);
    if (j == string::npos) {
      parts.push_back(s.substr(i));
      break;
    } else {
      if (i == j) {
        i++;
      } else {
        parts.push_back(s.substr(i, j - i));
        i = j + 1;
      }
    }
  }
  return parts;
}

bool process_cmd(const vector<string> &args, KeyValueStore *kv) {
  const auto &cmd = args[0];
  if (cmd == "set") {
    if (args.size() != 3) return false;
    kv->set(args[1], args[2]);
    return true;
  }
  if (cmd == "get") {
    if (args.size() != 2) return false;
    string s;
    if (kv->get(args[1], &s)) {
      cout << s << endl;
    } else {
      cout << "NULL" << endl;
    }
    return true;
  }
  if (cmd == "del") {
    if (args.size() != 2) return false;
    kv->del(args[1]);
    return true;
  }
  if (cmd == "count") {
    if (args.size() != 2) return false;
    cout << kv->count(args[1]) << endl;
    return true;
  }
  if (cmd == "begin") {
    if (args.size() != 1) return false;
    kv->begin();
    return true;
  }
  if (cmd == "rollback") {
    if (args.size() != 1) return false;
    if (!kv->rollback()) {
      cout << "NO TRANSACTION" << endl;
    }
    return true;
  }
  if (cmd == "commit") {
    if (args.size() != 1) return false;
    if (!kv->commit()) {
      cout << "NO TRANSACTION" << endl;
    }
    return true;
  }
  if (cmd == "end") {
    if (args.size() != 1) return false;
    return true;
  }
  return false;
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  // test1();
  // test2();
  // test3();
  // test4();
  // test5();
  // test6();
  KeyValueStore kv;
  while (!cin.eof()) {
    cout << ">> ";
    string cmd;
    getline(cin, cmd);
    auto args = split(cmd);
    if (args.empty() || !process_cmd(args, &kv)) {
      cout << "INVALID COMMAND" << endl;
    } else {
      if (args[0] == "end") break;
    }
  }
  return 0;
}
