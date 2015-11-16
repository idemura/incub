#include "template.hxx"

namespace iped {
namespace {

class TemplateDictImpl;
class TemplateImpl;

struct Value {
  enum Type {
    kString,
    kVector,
  };

  Type type = kString;
  void *p = nullptr;

  string* as_string() const { return reinterpret_cast<string*>(p); }
  std::vector<TemplateDictImpl*>* as_vector() const {
    return reinterpret_cast<std::vector<TemplateDictImpl*>*>(p);
  }

  void delete_ptr();
  bool no_data() const { return p == nullptr; }
};

void Value::delete_ptr() {
  if (p == nullptr) return;
  switch (type) {
    case kString: {
      delete as_string();
      break;
    }
    case kVector: {
      auto v = as_vector();
      for (auto p : *v) delete p;
      delete v;
      break;
    }
  }
}

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

  Instr() {}
  Instr(Op op, Substr s): op(op), s(s) {}
};

class TemplateDictImpl : public TemplateDict {
public:
  TemplateDictImpl() {}
  ~TemplateDictImpl() override;
  void set_string(Substr name, const std::string &value) override;
  TemplateDict* add_dict(Substr name) override;
  Value lookup(Substr name) const;

private:
  std::unordered_map<Substr, Value> map_;
  NON_COPYABLE(TemplateDictImpl);
};

TemplateDictImpl::~TemplateDictImpl() {
  for (const auto& kv : map_) {
    kv.second.delete_ptr();
  }
}

void TemplateDictImpl::set_string(Substr name, const std::string& value) {
  auto& v = map_[name];
  if (!v.no_data() && v.type == Value::kString) {
    *v.as_string() = value;
  } else {
    v.delete_ptr();
    v.type = Value::kString;
    v.p = new string(value);
  }
}

TemplateDict* TemplateDictImpl::add_map(Substr name) {
  auto& v = map_[name];
  if (v.type != Value::kVector) {
    v.delete_ptr();
  }
  v.type = Value::kVector;
  if (v.no_data()) {
    v.p = new std::vector<TemplateDictImpl*>;
  }
  v.as_vector()->emplace_back(new TemplateDictImpl>());
  return v.as_vector()->last().get();
}

Value TemplateDictImpl::lookup(Substr name) const {
  auto i = map_.find(name);
  if (map_.end() == i) {
    return {};
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
  bool expand(TemplateDict *dict, std::string *res) override;

private:
  std::vector<Instr> program_;
  NON_COPYABLE(TemplateImpl);
};

Value lookup_dict_stack(const std::vector<TemplateDictImpl*> &ds,
    Substr id,
    Value::Type type) {
  const Value empty;
  for (int i = ds.size(); i-- > 0; ) {
    auto v = ds[i]->lookup(id);
    if (!v.no_data()) {
      return v.type == type ? v : empty;
    }
  }
  return empty;
}

bool TemplateImpl::expand(TemplateDict *dict, std::string *res) override {
  std::vector<TemplateDictImpl*> ds{
    reinterpret_cast<TemplateDictImpl*>(dict)
  };
  for (auto &&instr : program_) {
    switch (instr.op) {
      case Instr::kString: {
        res->append(instr.s.data(), instr.s.size());
        break;
      }
      case Instr::kReplace: {
        auto v = lookup_dict_stack(ds, instr.s);
        if (v.no_data()) {
          return false;
        }
        res->append(*v.as_string());
        break;
      }
      case Instr::kVector: {
      }
      case Instr::kVectorNot: {
      }
      case Instr::kVectorPop: {
        break;
      }
    }
  }
  return true;
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
  c.program.emplace_back(Instr::kString, s.range(c.literal_first, i));
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
      c.program.emplace_back(Instr::kVector, id);
      c.rec_stack.push_back(id);
      break;
    case '!':
      c.program.emplace_back(Instr::kVectorNot, id);
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
      c.program.emplace_back(Instr::kVectorPop, Substr());
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

std::unique_ptr<TemplateDict> make_dict() {
  return new TemplateDictImpl();
}
}  // namespace

