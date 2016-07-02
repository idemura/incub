#include "ast.hxx"

namespace igor {

AST::AST(std::function<void(const string&)> error_handler)
  : error_(std::move(error_handler)) {
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
  if (error_) {
    error_(msg);
  } else {
    cerr<<msg<<endl;
  }
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

}  // namespace
