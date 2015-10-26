#include "template.hxx"

namespace iped {
namespace {

class TmplDictImpl;
class TemplateImpl;

using section = std::vector<TmplDictImpl*>;

struct Value {
  enum Type {
    kLiteral,
    kSection,
  };

  Type type = kLiteral;
  void *p = nullptr;

  Section* as_section() const { return reinterpret_cast<Section*>(p); }
  string* as_string() const { return reinterpret_cast<string*>(p); }
};

struct Instr {
  enum Op {
    kLiteral,
    kReplace,
    kSection,
    kSectionNot,
    kSectionPop,
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
  if (v.type == Value::kSection) {
    for (auto d : *v.as_section()) {
      delete_all(d);
    }
  } else {
    delete v.as_string();
  }
}

class TmplDictImpl : public TmplDict {
public:
  TmplDictImpl() {}
  ~TmplDictImpl() override { DeleteAll(map_); }
  void set_string(Substr name, const std::string &value) override;
  void add_map(Substr name) override;
  Value lookup(Substr name) const;

private:
  std::unordered_map<Substr, Value> map_;
  NON_COPYABLE(TmplDictImpl);
};

void TmplDictImpl::set_string(Substr name, const std::string& value) {
  auto& v = map_[name];
  if (v.p != nullptr) {
    delete_value(v);
  }
  v.type = Value::kLiteral;
  v.p = new string(value);
}

TmplDict* TmplDictImpl::add_map(Substr name) {
  auto& v = map_[name];
  if (v.type != Value::kSection) {
    DeleteValue(v);
  }
  v.type = Value::kSection;
  if (v.p == nullptr) {
    v.p = new MapRec();
  }
  auto m = new TmplDictImpl();
  v.as_section()->push_back(m);
  return m;
}

Value TmplDictImpl::lookup(Substr name) const {
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
  bool expand(TmplDict *dict, std::string *res) override;

private:
  std::vector<Instr> program_;
  NON_COPYABLE(TemplateImpl);
};

string* lookup_dict_stack(const std::vector<TmplDictImpl*> &ds, Substr id) {
  for (int i = ds.size(); i-- > 0; ) {
    auto v = ds[i]->lookup(id);
    if (v.p != nullptr) {
      if (v.type != Value::kLiteral) {
        // error
        return nullptr;
      }
      return v.as_literal();
    }
  }
  return nullptr;
}

bool TemplateImpl::expand(TmplDict *dict, std::string *res) override {
  std::vector<TmplDictImpl*> ds{
    reinterpret_cast<TmplDictImpl*>(dict)
  };
  for (auto &&instr : program_) {
    switch (instr.op) {
      case Instr::kLiteral:
        res->append();
        break;
      case Instr::kReplace:
        auto literal = lookup_dict_stack(ds, instr.s);
        if (literal == nullptr) return false;
        res->append(*literal);
        break;
      case Instr::kSection:
      case Instr::kSectionNot:
      case Instr::kSectionPop:
        break;
    }
  }
}

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
  c.program.emplace_back(Instr::kLiteral, s.range(c.literal_first, i));
  c.literal_first = -1;
}

bool parse_replace(Context &c, Substr s, int *i) {
  int j = *i + 1;
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
  c.program.emplace_back(Instr::kReplace, s.range(id_first, j));
  *i = j;
  return true;
}

bool parse_rec(Context &c, Substr s, int *i) {
  int j = *i + 1;
  const int op_index = j;
  if (!(s[j] == '$' || s[j] == '$' || s[j] == '$')) {
    c.error = "$, ! or / expected after #";
    return false;
  }
  j++;
  while (s[j] != '\n' && !is_space(s[j])) {
    j++;
  }
  auto id = s.range(op_index + 1, j);
  while (is_space(s[j])) j++;
  if (s[j] != '\n') {
    c.error = "# line should end with only spaces";
    return false;
  }
  switch (s[op_index]) {
    case '$':
      c.program.emplace_back(Instr::kSection, id);
      c.rec_stack.push_back(id);
      break;
    case '!':
      c.program.emplace_back(Instr::kSectionNot, id);
      c.rec_stack.push_back(id);
      break;
    case '/':
      if (c.rec_stack.empty()) {
        c.error = "Closing tag without open: " + std::to_string(id);
        return false;
      }
      if (c.rec_stack.back() != id) {
        c.error = "Closing tag " + std::to_string(id) + "mismatch open tag " +
                  std::to_string(c.rec_stack.back());
        return false;
      }
      c.rec_stack.pop_back();
      c.program.emplace_back(Instr::kSectionPop, Substr());
      break;
  }
  return true;
}

bool parse_line(Context &c, Substr s, int *i) {
  const int line_beginning = *i;
  int j = *i;
  while (is_space(s[j])) {
    j++;
  }
  if (s[j] == '\n') {
    // Just a blank line.
    if (c.literal_first < 0) c.literal_first = i;
    *i = j;
    return true;
  }
  if (c.s[j] == '#') {
    op_literal(c, line_beginning);
    *i = j;
    return parse_rec(c, s, i);
  }
  for (;;) {
    while (s[j] != '\n' && s[j] != '$') {
      j++;
    }
    if (s[j] == '\n') break;
    if (!parse_replace(c, s, &j)) {
      return false;
    }
  }
  *i = j;
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

std::unique_ptr<TmplDict> make_dict() {
  return new TmplDictImpl();
}
}  // namespace

