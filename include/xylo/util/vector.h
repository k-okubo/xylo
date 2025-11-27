
#ifndef XYLO_UTIL_VECTOR_H_
#define XYLO_UTIL_VECTOR_H_

#include <utility>
#include <vector>

namespace xylo {


// clang-format off
template <typename T>
class Vector {
 public:
  using base = std::vector<T>;
  using size_type = base::size_type;
  using reference = base::reference;
  using const_reference = base::const_reference;

 public:
  constexpr Vector() noexcept = default;
  constexpr ~Vector() = default;

  Vector(const Vector&) = delete;
  Vector& operator=(const Vector&) = delete;

  constexpr Vector(Vector&&) noexcept = default;
  constexpr Vector& operator=(Vector&&) noexcept = default;

  constexpr explicit Vector(size_type n) : impl_(n) {}
  constexpr Vector(size_type n, const T& value) : impl_(n, value) {}
  constexpr Vector(std::initializer_list<T> init) : impl_(init) {}
  constexpr Vector& operator=(std::initializer_list<T> init) { impl_ = init; return *this; }

  constexpr bool empty() const noexcept { return impl_.empty(); }
  constexpr size_type size() const noexcept { return impl_.size(); }
  constexpr reference operator[](size_type n) { return impl_[n]; }
  constexpr const_reference operator[](size_type n) const { return impl_[n]; }

  constexpr T* data() noexcept { return impl_.data(); }
  constexpr const T* data() const noexcept { return impl_.data(); }

  constexpr reference back() { return impl_.back(); }
  constexpr const_reference back() const { return impl_.back(); }

  constexpr void push_back(const T& x) { impl_.push_back(x); }
  constexpr void push_back(T&& x) { impl_.push_back(std::forward<T>(x)); }
  constexpr void pop_back() { impl_.pop_back(); }

  constexpr void clear() { impl_.clear(); }

  constexpr auto begin() noexcept { return impl_.begin(); }
  constexpr auto begin() const noexcept { return impl_.begin(); }

  constexpr auto end() noexcept { return impl_.end(); }
  constexpr auto end() const noexcept { return impl_.end(); }

  constexpr auto clone() const noexcept { return Vector<T>(this->impl_); }

 private:
  base impl_;

  explicit Vector(const base& impl) : impl_(impl) {}
};
// clang-format on


struct VectorHash {
  template <typename T>
  size_t operator()(const Vector<T>& v) const {
    size_t h = 0;
    for (auto& p : v) {
      h ^= std::hash<T>{}(p) + 0x9e3779b97f4a7c15ULL + (h << 6) + (h >> 2);
    }
    return h;
  }
};


}  // namespace xylo

#endif  // XYLO_UTIL_VECTOR_H_
