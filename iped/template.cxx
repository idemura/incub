#include "template.hxx"

namespace iped {
namespace {

class ValueMapImpl;
class TemplateImpl;

using MapRec = std::vector<ValueMapImpl*>;

struct Value {
  enum Type {
    kString,
    kRec,
  };

  Type type = kString;
  void *p = nullptr;

  MapRec* as_maprec() const { return reinterpret_cast<MapRec*>(p); }
  string* as_string() const { return reinterpret_cast<string*>(p); }
};

struct Instr {
  enum Op {
    kLiteral,
    kReplace,
    kRec,
  };

  Op op = kLiteral;
  Substr s;

  Instr(Op op, Substr s): op(op), s(s) {}
};

void delete_value(Value v);

void delete_all(const std::unordered_map<Substr, Value> &dict) {
  for (const auto& kv : dict) {
    delete_value(kv.second);
  }
}

void delete_value(Value v) {
  if (v.p == nullptr) return;
  if (v.type == Value::kRec) {
    for (auto d : *v.as_maprec()) {
      delete_all(d);
    }
  } else {
    delete v.as_string();
  }
}

class ValueMapImpl : public ValueMap {
public:
  ValueMapImpl() {}
  ~ValueMapImpl() override { DeleteAll(map_); }
  void set_string(Substr name, const std::string &value) override;
  void add_map(Substr name) override;
  Value get_value(Substr name) const;

private:
  std::unordered_map<Substr, Value> map_;
  NON_COPYABLE(ValueMapImpl);
};

void ValueMapImpl::set_string(Substr name, const std::string& value) {
  auto& v = map_[name];
  if (v.p != nullptr) {
    delete_value(v);
  }
  v.type = Value::kString;
  v.p = new string(value);
}

ValueMap* ValueMapImpl::add_map(Substr name) {
  auto& v = map_[name];
  if (v.type != Value::kRec) {
    DeleteValue(v);
  }
  v.type = Value::kRec;
  if (v.p == nullptr) {
    v.p = new MapRec();
  }
  auto m = new ValueMapImpl();
  v.as_maprec()->push_back(m);
  return m;
}

Value ValueMapImpl::get_value(Substr name) const {
  auto i = map_.find(name);
  if (i == map_.end()) {
    return Value();
  } else {
    return i->second;
  }
}

class TemplateImpl : public Template {
public:
  explicit TemplateImpl(std::vector<Instr> program):
    program_(std::move(program)) {
  }

  ~TemplateImpl() override {}
  bool expand(ValueMap *map) override;

private:
  std::vector<Instr> program_;
  NON_COPYABLE(TemplateImpl);
};

struct Context {
  std::string error;
  int literal_first = -1;
  int line = 1;
  std::vector<Instr> program;
  std::vector<Substr> rec_stack;
};

bool is_space(char c) {
  return c == ' ' || c == '\t' || c == '\v' || c == '\r';
}

void op_literal(Context &c, Substr s, int i) {
  if (c.literal_first < 0) return;
  c.program.emplace_back(Instr::kLiteral,
                         s.substr(c.literal_first, i - c.literal_first));
  c.literal_first = -1;
}

bool parse_replace(Context &c, Substr s, int *i) {
  int j = *i;
  j++;
  if (s[j] == '$') {
    op_literal(c, s, j);
    j++;
    c.literal_first = j;
    *i = j;
    return true;
  }
  if (s[j] != '(') {
    c.error = "$( expected";
    return false;
  }
  j++;
  int id_first = j;
  while (s[j] != '\n' && s[j] != ')') {
    if (s[j] < ' ' || s[j] >= 127) {
      c.error = "Invalid id character";
      return false;
    }
    j++;
  }
  if (s[j] == '\n') {
    c.error = "$( has no closing )";
    return false;
  }
  if (id_first + 1 == j) {
    c.error = "Empty id";
    return false;
  }
  j++;
  c.literal_first = j;
  c.program.emplace_back(Instr::kReplace,
                         s.substr(id_first, j - id_first));
  *i = j;
  return true;
}

bool parse_line(Context &c, Substr s, int i) {
  int j = i;
  while (is_space(s[i])) {
    i++;
  }
  if (s[i] == '\n') {
    // Just a blank line.
    if (c.literal_first < 0) c.literal_first = i;
    return true;
  }
  if (c.s[i] == '#') {
    op_literal(c, i);
    return parse_rec(c, s, i);
  }
  for (;;) {
    while (s[i] != '\n' && s[i] != '$') {
      i++;
    }
    if (!parse_replace(c, s, &i)) {
      return false;
    }
  }
  return true;
}
}  // namespace

std::unique_ptr<Template> make_template(Substr s) {
  if (s.empty() || s.last() != '\n') {
    cerr<<"Empty or not ends with new line"<<endl;
    return nullptr;
  }
  Context c;
  int i = 0;
  while (i < s.size()) {
    if (!parse_line(c, s, i)) {
      break;
    }
    while (s[i] != '\n') {
      i++;
    }
    i++;
    c.line++;
  }
  if (!c.error.empty()) {
    return nullptr;
  }
  // if literal is left, put into program.
  op_literal(c, s, s.size());
  return new TemplateImpl(std::move(c.program));
}
}  // namespace
