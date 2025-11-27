
#ifndef XYLO_UTIL_PAIR_HASH_H_
#define XYLO_UTIL_PAIR_HASH_H_

#include <utility>

namespace xylo {


struct PairHash {
  template <class T1, class T2>
  size_t operator()(const std::pair<T1, T2>& p) const {
    size_t h1 = std::hash<T1>{}(p.first);
    size_t h2 = std::hash<T2>{}(p.second);

    return h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6) + (h1 >> 2));
  }
};


}  // namespace xylo

#endif  // XYLO_UTIL_PAIR_HASH_H_
