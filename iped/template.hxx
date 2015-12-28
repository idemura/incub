#ifndef IPED_TEMPLATE_HXX
#define IPED_TEMPLATE_HXX

#include "base.hxx"

namespace iped {

class TemplateDict {
public:
  virtual ~TemplateDict() = default;
  virtual void set_string(Substr name, string value) = 0;
  virtual TemplateDict* add_dict(Substr name) = 0;
};

class Template {
public:
  virtual ~Template() {}
  virtual bool expand(TemplateDict *dict, std::string *res) const = 0;
};

std::unique_ptr<Template> make_template(Substr s);
std::unique_ptr<TemplateDict> make_dict();

} // namespace

#endif
