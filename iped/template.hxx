#ifndef IPED_TEMPLATE_HXX
#define IPED_TEMPLATE_HXX

#include "base.hxx"

namespace iped {

class ValueMap {
public:
  virtual ~ValueMap() {}
  virtual void set_string(Substr name, const std::string &value) = 0;
  virtual ValueMap* add_map(Substr name) = 0;
};

class Template {
public:
  virtual ~Template() {}
  virtual bool expand(ValueMap *map) const = 0;
};

std::unique_ptr<Template> make_template(Substr s);

}  // namespace

#endif
