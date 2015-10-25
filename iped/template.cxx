#include "template.hxx"

namespace iped {

std::unique_ptr<Template> Template::New(Substr s) {
  /*
   state machine. 
     1. walk spaces. find # or not #.
     2. # read to end: %id, !id, $id and \n. push/pop from stack.
     3. //
   */
  while (!s.empty()) {
    auto t = s;
    if (s.front() == ' ' || 
  }

}

}  // namespace
