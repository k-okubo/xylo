
#ifndef XYLO_UTIL_SET_H_
#define XYLO_UTIL_SET_H_

#include <functional>
#include <unordered_set>
#include <utility>

namespace xylo {


// clang-format off
template <class Key, class Hash = std::hash<Key>, class Pred = std::equal_to<Key>>
class Set {
 public:
  using base = std::unordered_set<Key, Hash, Pred>;
  using key_type = base::key_type;
  using value_type = base::value_type;
  using size_type = base::size_type;
  using iterator = base::iterator;
  using const_iterator = base::const_iterator;

  Set() = default;
  ~Set() = default;

  Set(const Set&) = delete;
  Set& operator=(const Set&) = delete;

  Set(Set&&) = default;
  Set& operator=(Set&&) = default;

  Set(std::initializer_list<value_type> init) : impl_(init) {}
  Set& operator=(std::initializer_list<value_type> init) { impl_ = init; return *this; }

  bool empty() const noexcept { return impl_.empty(); }
  size_type size() const noexcept { return impl_.size(); }
  bool contains(const Key& x) const { return impl_.contains(x); }

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

#endif  // XYLO_UTIL_SET_H_
