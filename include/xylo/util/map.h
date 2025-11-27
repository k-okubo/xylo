
#ifndef XYLO_UTIL_MAP_H_
#define XYLO_UTIL_MAP_H_

#include <functional>
#include <unordered_map>
#include <utility>

namespace xylo {


// clang-format off
template <class Key, class T, class Hash = std::hash<Key>, class Pred = std::equal_to<Key>>
class Map {
 public:
  using base = std::unordered_map<Key, T, Hash, Pred>;
  using key_type = base::key_type;
  using value_type = base::value_type;
  using size_type = base::size_type;
  using iterator = base::iterator;
  using const_iterator = base::const_iterator;

  Map() = default;
  ~Map() = default;

  Map(const Map&) = delete;
  Map& operator=(const Map&) = delete;

  Map(Map&&) = default;
  Map& operator=(Map&&) = default;

  Map(std::initializer_list<value_type> init) : impl_(init) {}
  Map& operator=(std::initializer_list<value_type> init) { impl_ = init; return *this; }

  bool empty() const noexcept { return impl_.empty(); }
  size_type size() const noexcept { return impl_.size(); }
  bool contains(const Key& x) const { return impl_.contains(x); }
  T& at(const Key& x) { return impl_.at(x); }
  const T& at(const Key& x) const { return impl_.at(x); }

  template <class... Args>
  auto emplace(Args&&... args) { return impl_.emplace(std::forward<Args>(args)...); }

  auto erase(const Key& x) { return impl_.erase(x); }
  auto erase(iterator position) { return impl_.erase(position); }
  auto erase(const_iterator position) { return impl_.erase(position); }

  void clear() noexcept { impl_.clear(); }

  auto find(const Key& x) { return impl_.find(x); }
  auto find(const Key& x) const { return impl_.find(x); }

  template <class K>
  auto find(const K& x) { return impl_.find(x); }
  template <class K>
  auto find(const K& x) const { return impl_.find(x); }

  auto begin() noexcept { return impl_.begin(); }
  auto begin() const noexcept { return impl_.begin(); }

  auto end() noexcept { return impl_.end(); }
  auto end() const noexcept { return impl_.end(); }

 private:
  base impl_;
};
// clang-format on


}  // namespace xylo

#endif  // XYLO_UTIL_MAP_H_
