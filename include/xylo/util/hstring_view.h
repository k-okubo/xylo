
#ifndef XYLO_UTIL_HSTRING_VIEW_H_
#define XYLO_UTIL_HSTRING_VIEW_H_

#include "xylo/util/hstring.h"

namespace xylo {


// clang-format off
class HStringView {
 public:
  using charT = char;
  using size_type = size_t;

  constexpr explicit HStringView(const charT* str) noexcept {
    auto cur = str;

    // djb2 hash
    auto hash = kInitHash;
    while (*cur != '\0') {
      hash = advance_hash(hash, *(cur++));
    }

    data_ = str;
    size_ = cur - str;
    hash_ = hash;
  }

  constexpr HStringView(const charT* str, size_type len) noexcept {
    auto cur = str;
    auto end = str + len;

    // djb2 hash
    auto hash = kInitHash;
    while (cur != end) {
      hash = advance_hash(hash, *(cur++));
    }

    data_ = str;
    size_ = len;
    hash_ = hash;
  }

  constexpr size_type hash() const noexcept { return hash_; }

  constexpr HString to_str() const { return HString(data_, size_); }

  friend constexpr bool operator==(const HStringView& lhs, const HStringView& rhs) noexcept {
    return lhs.hash_ == rhs.hash_ && lhs.size_ == rhs.size_ && std::memcmp(lhs.data_, rhs.data_, lhs.size_) == 0;
  }

  friend constexpr bool operator!=(const HStringView& lhs, const HStringView& rhs) noexcept {
    return !(lhs == rhs);
  }

  constexpr bool equals(const HString& rhs) const noexcept {
    return hash_ == rhs.hash() && size_ == rhs.size() && std::memcmp(data_, rhs.data(), size_) == 0;
  }

 private:
  static constexpr size_type kInitHash = 5381;

  const charT* data_;
  size_type size_;
  size_type hash_;

  constexpr size_type advance_hash(size_type hash, charT c) const noexcept {
    // djb2 hash
    return (hash << 5) + hash + static_cast<unsigned char>(c);
  }
};
// clang-format on


struct StringViewHash {
  size_t operator()(const HStringView& s) const { return s.hash(); }
};


}  // namespace xylo

#endif  // XYLO_UTIL_HSTRING_VIEW_H_
