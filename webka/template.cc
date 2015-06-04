#include "template.h"

namespace webka {

class TemplateImpl : public Template {
public:
  ~TemplateImpl() override {}
  bool apply(const VarStore &vs) const override;

private:
  NON_COPYABLE(TemplateImpl);
};

bool TemplateImpl::apply(const VarStore &vs) const {
  return false;
}

/*
# define
#   struct Person {
#     name: String
#     salary: i32
#   }
#   n: i32
#   m: i64
#   title: string
#   f: float32
#   d: float64
#   b: bool
#   l: String[] // vector<string>
#   m: String[i32](map) // map/unordered_map<i32, string>
#   person: Person[]


title: %title%
# for r : person
  <p>%r.name% <b>%r.salary%</b></p>
# end
# if b
haha!
# else
debug.
# end

generates C++ code:
struct Person {
  string name;
  int salary = 0;
};

my_templ_1.tmpl -> MyTemp1
struct MyTemp1 {
  int n = 0;
  // ...
};

// Error?
string my_templ_1(const MyTempl1 &ctx) {
  string s;
  s.append("title: ");
  s.append(ctx.title);
  s.append("\n");  // flag preserve whitespaces
  for (auto &r : ctx.person) {
    s.append("  <p>");
    s.append(r.name);
    // ...
    s.append(atoi(r.salary));
  }
  return s;
}
*/

}  // namespace webka
