
#ifndef XYLO_UTIL_DOWNCASTABLE_H_
#define XYLO_UTIL_DOWNCASTABLE_H_

#include "xylo/util/contract.h"

namespace xylo {


class Downcastable {
 public:
  virtual ~Downcastable() = default;

  template <typename T>
  T* As() {
    auto t = dynamic_cast<T*>(this);
    xylo_contract(t != nullptr);
    return t;
  }

  template <typename T>
  const T* As() const {
    auto t = dynamic_cast<const T*>(this);
    xylo_contract(t != nullptr);
    return t;
  }
};


}  // namespace xylo

#endif  // XYLO_UTIL_DOWNCASTABLE_H_
