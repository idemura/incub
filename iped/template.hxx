#ifndef IPED_TEMPLATE_HXX
#define IPED_TEMPLATE_HXX

#include "base.hxx"

namespace iped {

class TemplateDict {
public:
  TemplateDict() {}
  ~TemplateDict();
  void SetString(Substr name, const std::string& value);
  TemplateDict* AddDict(Substr name);

private:
  enum class Type {
    kString,
    kVectorOfDict,
  };

  using VectorOfDict = std::vector<TemplateDict*>;

  struct Value {
    Type type = Type::kString;
    void *p = nullptr;
    VectorOfDict* as_vector() const { return reinterpret_cast<VectorOfDict*>(p); }
    string* as_string() const { return reinterpret_cast<string*>(p); }
  };

  static void DeleteAll(const std::unordered_map<Substr, Value> &dict);
  static void DeleteValue(Value v);

  std::unordered_map<Substr, Value> dict_;
  NON_COPYABLE(TemplateDict);
};

class Template {
public:
  std::unique_ptr<Template> New(Substr s);

private:
  enum InstrType {
    kLiteral,
    kSubst,
    kDict,
  }
  struct Instr {
    Instr instr = kLiteral;
    Substr s;
  };

  std::vector<Instr> program_;
};

}  // namespace

#endif
