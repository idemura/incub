#ifndef IPED_TEMPLATE_HXX
#define IPED_TEMPLATE_HXX

#include "base.hxx"

namespace iped {

class TemplateArgs {
public:
  TemplateArgs();
  void ShowSection(const char *name);

private:

};

class Template {
public:
  std::unique_ptr<Template> New(Substr s);

private:
};

}  // namespace

#endif
