#include "template.h"

namespace webka {

class TemplateImpl : public Template {
public:
  ~TemplateImpl() override {}
  bool apply(const VarStore &vs) const override;

private:
  NON_COPYABLE(TemplateImpl);
};

bool TemplateImpl::apply(const VarStore &vs) const {
}

}  // namespace webka
