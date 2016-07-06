#include "ast.hxx"

namespace igor {

AST::AST(std::function<void(const string&)> error_handler)
  : error_(std::move(error_handler)) {
  if (!error_) {
    error_ = [](const string& msg) {
      cerr<<msg<<endl;
    };
  };
}

AST::~AST() {
  clear_intern();
  for (auto p : function_map_) {
    delete p.second;
  }
}

void AST::reset() {
  function_map_.clear();
}

void AST::error(const string& msg) {
  error_(msg);
}

bool AST::add_function(AstFunction *f) {
  if (function_map_.end() == function_map_.find(f->name)) {
    function_map_.emplace(f->name, f);
    return true;
  }
  error("Function " + f->name + " already defined");
  return false;
}

// With const string& I have compiler errors about const/non-const.
string *AST::intern(string s) {
  const auto i = name_intern_.find(&s);
  if (i == name_intern_.end()) {
    return *name_intern_.insert(new string(std::move(s))).first;
  }
  return *i;
}

void AST::clear_intern() {
  for (auto p : name_intern_) delete p;
  name_intern_.clear();
}

string AstType::to_string() const {
  std::stringstream ss;
  ss<<name;
  if (!args.empty()) {
    ss<<"(";
    for (size_t i = args.size(); i-- > 0;) {
      ss<<args[i]->to_string();
      if (i > 0) ss<<", ";
    }
    ss<<")";
  }
  return ss.str();
}

}  // namespace
