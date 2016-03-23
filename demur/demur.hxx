#ifndef IGOR_DEMUR_HXX
#define IGOR_DEMUR_HXX

#include "base.hxx"

namespace igor {

class Flags: public FlagSet {
 public:
  i32 port = 8060;
  string document_root = ".";
  i32 poll_ms = 20;

  Flags() {
    register_flag("port", &port);
    register_flag("document_root", &document_root);
    register_flag("poll_ms", &poll_ms);
  }
};

extern std::unique_ptr<Flags> flags;

}  // namespace

#endif
