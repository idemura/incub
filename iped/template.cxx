#include "template.hxx"

namespace iped {
namespace {
bool is_space(char c) {
  return c == ' ' || c == '\t' || c == '\v';
}
}  // namespace

TemplateDict::~TemplateDict() {
  DeleteAll(dict_);
}

void TemplateDict::DeleteValue(Value v) {
  if (v.p == nullptr) return;
  if (v.type == Type::kVectorOfDict) {
    for (auto d : *v.as_vector()) {
      DeleteAll(d);
    }
  } else {
    delete v.as_string();
  }
}

void TemplateDict::DeleteAll(const std::unordered_map<Substr, Value> &dict) {
  for (const auto& kv : dict) {
    DeleteValue(kv.second);
  }
}

void TemplateDict::SetString(Substr name, const std::string& value) {
  auto& v = dict_[name];
  if (v.p != nullptr) {
    DeleteValue(v);
  }
  v.type = Type::kString;
  v.p = new string(value);
}

TemplateDict* TemplateDict::AddDict(Substr name) {
  auto& v = dict_[name];
  if (v.type != Type::kList) {
    DeleteValue(v);
  }
  v.type = Type::kList;
  if (v.p == nullptr) {
    v.p = new std::vector<TemplateDict*>();
  }
  auto d = new TemplateDict();
  v.as_vector()->push_back(d);
  return d;
}

std::unique_ptr<Template> Template::New(Substr s) {
  /*
   state machine.
     1. walk spaces. find # or not #.
     2. # read to end: %id, !id, $id and \n. push/pop from stack.
     3. //
   */
  // Loop for every line.
  std::vector<Instr> program;
  Substr plain_s(s.data(), 0);
  while (!s.empty()) {
    int i = 0;
    while (i < s.size() && is_space(s[i])) {
      i++;
    }
    if (i == s.size()) {
      plain_s.expand_r(s.size());
      break;
    }
    if (s[i] == '\n') {
      plain_s.expand_r(i + 1);
      s.shrink_l(i + 1);
      continue;
    }
    if (s[i] == '#') {

    } else {
      while (i < s.size() && s[i] != '\n') {
        i++;
      }
      // check i == size or \n
    }
  }
  return nullptr;
}

}  // namespace
