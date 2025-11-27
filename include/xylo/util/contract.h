
#ifndef XYLO_UTIL_CONTRACT_H_
#define XYLO_UTIL_CONTRACT_H_

#include <cassert>
#include <exception>

namespace xylo {


#ifdef NDEBUG

#define xylo_unreachable() std::terminate()
#define xylo_contract(condition)     \
  do {                               \
    if (!(condition)) [[unlikely]] { \
      xylo_unreachable();            \
    }                                \
  } while (0)

#define xylo_check(proc)        \
  do {                          \
    if (!(proc)) [[unlikely]] { \
      xylo_unreachable();       \
    }                           \
  } while (0)

#else

#define xylo_unreachable() assert(false && "unreachable here")
#define xylo_contract(condition) assert(condition)
#define xylo_check(proc) assert(proc)

#endif


}  // namespace xylo

#endif  // XYLO_UTIL_CONTRACT_H_
