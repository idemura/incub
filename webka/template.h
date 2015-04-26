#ifndef TEMPLATE_H
#define TEMPLATE_H

// In future, make this template pure standalone.
#include "base.h"

#include <functional>
#include <string>

namespace webka {
using std::string;

using i64 = long long int;
using i32 = int;

class RecordType {
public:
  //
};

class VarStore {
public:
  // This add methods rely on C++11 move semantics.
  void add_i32(string name, i32 value);
  void add_i64(string name, i64 value);
  void add_string(string name, string value);

private:
  NON_COPYABLE(Template);
};

class Template {
public:
  virtual bool apply(const VarStore &vs) const = 0;
  virtual ~Template() {}
};

std::unique_ptr<Template> parse(const string &tmpl);

}  // namespace webka

#endif // TEMPLATE_H
