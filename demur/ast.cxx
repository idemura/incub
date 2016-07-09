#include "ast.hxx"

namespace igor {

ErrorSink::Formatter::Formatter(ErrorSink *sink, int line, int column)
  : sink_(sink),
    line_(line),
    column_(column),
    ss_(new std::stringstream()) {
}

void ErrorSink::print_to_stderr(bool locations) const {
  auto &os = cerr;
  for (const auto &e : errors_) {
    if (locations) {
      os<<file_;
      if (e->line > 0) {
        os<<":"<<e->line;
        if (e->column > 0) os<<"@"<<e->column;
      }
      os<<": ";
    }
    os<<e->msg<<endl;
  }
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

std::unique_ptr<AstType> AstType::clone() const {
  auto p = std::make_unique<AstType>();
  p->name = name;
  p->carry = carry;
  for (const auto &a : args) {
    p->args.push_back(a->clone());
  }
  return p;
}

void AST::reset() {
  functions_.clear();
}

void AST::error(int line, int column, const string& msg) {
  *es_->format_err(line, column)<<msg;
}

void AST::add_function(std::unique_ptr<AstFunction> f) {
  functions_.push_back(std::move(f));
}

bool AST::analyze_semantic() {
  std::unordered_set<string> fn_map;
  for (const auto &f : functions_) {
    if (!fn_map.insert(f->name).second) {
      *es_->format_err(0, 0)<<"function '"<<f->name<<"' duplicated";
    }
    analyze_function(f.get());
  }
  return es_->err_count() == 0;
}

void AST::analyze_function(AstFunction *f) {
  // Analyze args.
  if (!f->arg_list->args.empty()) {
    // Remember, args are stores in reverse order.
    auto a_last = f->arg_list->args[0].get();
    if (a_last->type == nullptr) {
      *es_->format_err(0, 0)<<"in function '"<<f->name<<"': argument '"
                            <<a_last->name<<"' missing type spec";
    } else {
      auto arg_pos = f->arg_list->args.size();
      for (auto &a : f->arg_list->args) {
        if (a->type == nullptr) {
          a->type = a_last->type->clone();
        } else {
          a_last = a.get();
        }
        if (a->name.empty()) {
          *es_->format_err(0, 0)<<"in function '"<<f->name<<"': "
                                <<"unnamed argment in position "<<arg_pos;
        }
        arg_pos--;
      }
    }
  }
  if (!f->ret_list->args.empty()) {
    // Remember, args are stores in reverse order.
    auto a_last = f->ret_list->args[0].get();
    if (a_last->type == nullptr) {
      *es_->format_err(0, 0)<<"in function '"<<f->name<<"': return variable '"
                            <<a_last->name<<"' missing type spec";
    } else {
      auto arg_pos = f->ret_list->args.size();
      for (auto &a : f->ret_list->args) {
        if (a->type == nullptr) {
          a->type = a_last->type->clone();
        } else {
          a_last = a.get();
        }
        // Empty name is allowed in return spec. In this case, return works
        // pretty much as function with named (with defaults) args.
        //
        // For example:
        // fn foo(): Int, s: String, Int {
        //   s = "hello";  // Assigned to default("") if no explicit.
        //   return 10, 20;
        // }
        //
        // Another extreme of this is all named:
        // fn foo(): x: Int, y: Int {
        //   x = 10;
        //   y = 20;
        //   return;  // Empty, no positional returns.
        // }
        arg_pos--;
      }
    }
  }
}

}  // namespace
