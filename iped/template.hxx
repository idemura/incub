#ifndef IPED_TEMPLATE_HXX
#define IPED_TEMPLATE_HXX

#include "base.hxx"

namespace iped {

class TmplDict {
public:
  virtual ~TmplDict() {}
  virtual void set_string(Substr name, const std::string &value) = 0;
  virtual TmplDict* add_dict(Substr name) = 0;
};

class Template {
public:
  virtual ~Template() {}
  virtual bool expand(TmplDict *dict, std::string *res) const = 0;
};

std::unique_ptr<Template> make_template(Substr s);
std::unique_ptr<TmplDict> make_dict();

}  // namespace

#endif
