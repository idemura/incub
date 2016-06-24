#include "ast.hxx"

namespace igor {

AST::AST(std::function<void(const string&)> error_handler)
  : error_(std::move(error_handler)) {
}

AST::~AST() {
  clear_intern();
}

void AST::error(const string& msg) {
  if (error_) {
    error_(msg);
  } else {
    cerr<<msg<<endl;
  }
}

bool AST::add_function(Function *function) {
  auto function_up = wrap_unique(function);
  if (function_map_.find(function->name) == function_map_.end()) {
    function_map_.emplace(function->name, std::move(function_up));
    return true;
  }
  error("Function " + function->name + " already defined");
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
