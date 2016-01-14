#include "template.hxx"

namespace iped {
namespace {

class TemplateDictImpl;
class TemplateImpl;

// Value is copyable, but it doesn't free memory in destructor.
// Use delete_rec to delete a subtree.
struct Value {
  enum Type {
    kString,
    kVector,
    kNone,
  };
  virtual ~Value() = default;
  virtual Type type() const { return kNone; }
};

struct ValueString : public Value {
  ~ValueString() override {}
  Type type() const override { return kString; }

  string s;
};

struct ValueVector : public Value {
  ~ValueVector() override {}
  Type type() const override { return kVector; }

  std::vector<std::unique_ptr<TemplateDictImpl>> v;
};

// Template dictionary implementaion.
class TemplateDictImpl : public TemplateDict {
public:
  TemplateDictImpl() {}
  ~TemplateDictImpl() override {}
  void set_string(Substr name, string value) override;
  TemplateDict* add_dict(Substr name) override;
  Value* lookup(Substr name) const;

private:
  std::unordered_map<Substr, std::unique_ptr<Value>> map_;
  NON_COPYABLE(TemplateDictImpl);
};

// Template implementation: instruction, stack frame and etc.
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

struct StackFrame {
  TemplateDictImpl *dict = nullptr;
  StackFrame *const prev;

  explicit StackFrame(StackFrame *prev): prev(prev) {}
};

class TemplateImpl : public Template {
public:
  explicit TemplateImpl(std::vector<Instr> program):
    program_(std::move(program)) {
  }

  ~TemplateImpl() override {}
  bool expand(TemplateDict *dict, string *res) const override;

private:
  bool expand_impl(
      TemplateDictImpl *dict,
      StackFrame *prev_sf,
      string* res) const;

  std::vector<Instr> program_;
  NON_COPYABLE(TemplateImpl);
};

void TemplateDictImpl::set_string(Substr name, string value) {
  auto &v = map_[name];
  if (v->type() != Value::kString) {
    v.reset(new ValueString);
  }
  auto val = reinterpret_cast<ValueString*>(v.get());
  val->s = std::move(value);
}

TemplateDict* TemplateDictImpl::add_dict(Substr name) {
  auto &v = map_[name];
  if (v->type() != Value::kVector) {
    v.reset(new ValueVector);
  }
  auto val = reinterpret_cast<ValueVector*>(v.get());
  val->v.emplace_back(new TemplateDictImpl);
  return val->v.back().get();
}

Value* TemplateDictImpl::lookup(Substr name) const {
  auto i = map_.find(name);
  if (map_.end() == i) {
    return nullptr;
  } else {
    return i->second.get();
  }
}

Value* lookup_dict_stack(
    StackFrame *sf,
    Substr id,
    Value::Type type) {
  while (nullptr != sf) {
    auto v = sf->dict->lookup(id);
    if (nullptr != v) {
      if (v->type() != type)
        return nullptr;
      else
        return v;
    }
    sf = sf->prev;
  }
  return nullptr;
}

bool TemplateImpl::expand(TemplateDict *dict, string *res) const {
  return expand_impl(reinterpret_cast<TemplateDictImpl*>(dict), nullptr, res);
}

bool TemplateImpl::expand_impl(
    TemplateDictImpl *dict,
    StackFrame *prev_sf,
    string *res) const {
  StackFrame sf(prev_sf);
  sf.dict = reinterpret_cast<TemplateDictImpl*>(dict);
  for (auto &&instr : program_) {
    auto pop = false;
    switch (instr.op) {
      case Instr::kLiteral: {
        res->append(instr.s.data(), instr.s.size());
        break;
      }
      case Instr::kReplace: {
        auto v = lookup_dict_stack(&sf, instr.s, Value::kString);
        if (nullptr == v) {
          return false;
        }
        res->append(reinterpret_cast<ValueString*>(v)->s);
        break;
      }
      case Instr::kSection: {
        auto v = lookup_dict_stack(&sf, instr.s, Value::kVector);
        if (nullptr == v) {
          return false;
        }
        auto val = reinterpret_cast<ValueVector*>(v);
        for (auto &&sub_dict : val->v) {
          if (!expand_impl(sub_dict.get(), &sf, res)) {
            return false;
          }
        }
        break;
      }
      case Instr::kSectionNot: {
        auto v = lookup_dict_stack(&sf, instr.s, Value::kVector);
        if (nullptr == v) {
          return false;
        }
        auto val = reinterpret_cast<ValueVector*>(v);
        break;
      }
      case Instr::kSectionPop: {
        pop = true;
        break;
      }
    }
    if (pop) break;
  }
  return true;
}

struct Context {
  string error;
  int literal_first = -1;
  int line = 1;
  std::vector<Instr> program;
  std::vector<Substr> rec_stack;
};

bool is_space(char c) {
  return c == ' ' || c == '\t' || c == '\v' || c == '\r';
}

void op_literal(Context &c, Substr s, int end_pos) {
  if (c.literal_first < 0) return;
  c.program.emplace_back(Instr::kLiteral, s.range(c.literal_first, end_pos));
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
  const int line_begin = *i;
  int j = *i;
  while (is_space(s[j])) {
    j++;
  }
  if (s[j] == '\n') {
    // Just a blank line.
    if (c.literal_first < 0) c.literal_first = *i;
    *i = j;
    return true;
  }
  if (s[j] == '#') {
    op_literal(c, s, line_begin);
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
} // namespace

std::unique_ptr<Template> make_template(Substr s) {
  if (s.empty() || s.back() != '\n') {
    cerr<<"Empty or not ends with new line"<<endl;
    return nullptr;
  }
  Context c;
  int i = 0;
  while (i < s.size()) {
    if (!parse_line(c, s, &i)) {
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
  // If literal is left, put into program.
  op_literal(c, s, s.size());
  return std::make_unique<TemplateImpl>(std::move(c.program));
}

std::unique_ptr<TemplateDict> make_dict() {
  return std::make_unique<TemplateDictImpl>();
}
} // namespace

