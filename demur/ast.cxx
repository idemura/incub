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
  reset();
}

void AST::reset() {
  functions_.clear();
}

void AST::error(const string& msg) {
  error_(msg);
}

void AST::add_function(std::unique_ptr<AstFunction> f) {
  functions_.push_back(std::move(f));
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
