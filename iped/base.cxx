#include "base.hxx"

#include <city.h>

namespace iped {

u64 Substr::hash() const {
  return CityHash64(data_, size_);
}

}  // namespace
