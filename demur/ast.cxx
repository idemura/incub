#include "ast.hxx"

namespace igor {

ErrorSink::ErrorSink(Args args)
  : stream_(args.stream),
    file_(args.file.empty()? "<unknown>": args.file),
    report_location_(args.report_location),
    on_error_(std::move(args.on_error)) {
}

void ErrorSink::error(int line, int column, const string &msg) {
  err_count_++;
  if (stream_ != nullptr) {
    if (report_location_) {
      if (!file_.empty()) *stream_<<file_<<":";
      *stream_<<line;
      if (column > 0) *stream_<<"@"<<column;
      *stream_<<": ";
    }
    *stream_<<msg<<endl;
  }
  if (on_error_) on_error_(line, column, msg);
}

AST::AST(ErrorSink *es): es_(es) {}

AST::~AST() {
  reset();
}

void AST::reset() {
  functions_.clear();
}

void AST::error(int line, int column, const string& msg) {
  es_->error(line, column, msg);
}

void AST::add_function(std::unique_ptr<AstFunction> f) {
  functions_.push_back(std::move(f));
}

bool AST::analyze_semantic() {
  return true;
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
