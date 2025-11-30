
#ifndef XYLO_UTIL_HSTRING_H_
#define XYLO_UTIL_HSTRING_H_

#include <cstring>
#include <iostream>
#include <string>
#include <utility>

namespace xylo {


// clang-format off
class HString {
 public:
  using charT = char;
  using size_type = size_t;
  static_assert(sizeof(charT) == 1);

  constexpr HString() noexcept : data_(nullptr), capacity_(0), size_(0), hash_(kInitHash) {}

  constexpr explicit HString(const charT* s) : HString(s, std::char_traits<charT>::length(s)) {}

  constexpr HString(const charT* s, size_type n) : capacity_(n), size_(n) {
    auto p = new charT[n];
    data_ = p;
    auto end = s + n;
    auto hash = kInitHash;
    while (s != end) {
      auto c = *s;
      *(p++) = c;
      hash = advance_hash(hash, c);
      ++s;
    }
    hash_ = hash;
  }

  constexpr ~HString() {
    release_data();
  }

  HString(const HString&) = delete;
  HString& operator=(const HString&) = delete;

  constexpr HString(HString&& rhs) noexcept {
    move_data(std::move(rhs));
  }

  constexpr HString& operator=(HString&& rhs) noexcept {
    release_data();
    move_data(std::move(rhs));
    return *this;
  }

  constexpr size_type size() const noexcept { return size_; }
  constexpr size_type hash() const noexcept { return hash_; }
  constexpr const charT* data() const noexcept { return data_; }

  constexpr void push_back(charT c) {
    if (data_ == nullptr) {
      capacity_ = kInitCapacity;
      data_ = new charT[kInitCapacity];

    } else if (size_ >= capacity_) {
      auto new_capacity = capacity_ << 1;
      auto new_data = new charT[new_capacity];
      std::memcpy(new_data, data_, size_);

      release_data();
      capacity_ = new_capacity;
      data_ = new_data;
    }

    data_[size_] = c;
    size_ += 1;

    hash_ = advance_hash(hash_, c);
  }

  constexpr HString& append(const charT* s, size_type n) {
    if (data_ == nullptr) {
      capacity_ = n > kInitCapacity ? n : kInitCapacity;
      data_ = new charT[capacity_];

    } else if (size_ + n > capacity_) {
      auto new_capacity = std::bit_ceil(size_ + n);
      auto new_data = new charT[new_capacity];
      std::memcpy(new_data, data_, size_);

      release_data();
      capacity_ = new_capacity;
      data_ = new_data;
    }

    auto p = data_ + size_;
    auto end = s + n;
    auto hash = hash_;
    while (s != end) {
      auto c = *s;
      *(p++) = c;
      hash = advance_hash(hash, c);
      ++s;
    }
    size_ += n;
    hash_ = hash;

    return *this;
  }

  friend constexpr bool operator==(const HString& lhs, const HString& rhs) noexcept {
    return lhs.hash_ == rhs.hash_ && lhs.size_ == rhs.size_ && std::memcmp(lhs.data_, rhs.data_, lhs.size_) == 0;
  }

  friend constexpr bool operator!=(const HString& lhs, const HString& rhs) noexcept {
    return !(lhs == rhs);
  }

  friend std::ostream& operator<<(std::ostream& stream, const HString& string) {
    return stream.write(string.data_, string.size_);
  }

  HString& operator+=(const HString& rhs) {
    return append(rhs.data_, rhs.size_);
  }

  HString& operator+=(const charT* rhs) {
    return append(rhs, std::char_traits<charT>::length(rhs));
  }

  HString& operator+=(const std::string& rhs) {
    return append(rhs.data(), rhs.size());
  }

  constexpr std::string cpp_str() const {
    return std::string(data_, size_);
  }

  constexpr std::string_view cpp_view() const {
    return std::string_view(data_, size_);
  }

 private:
  static constexpr size_type kInitCapacity = 16;
  static constexpr size_type kInitHash = 5381;

  charT* data_;
  size_type capacity_;
  size_type size_;
  size_type hash_;

  constexpr size_type advance_hash(size_type hash, charT c) const noexcept {
    // djb2 hash
    return (hash << 5) + hash + static_cast<unsigned char>(c);
  }

  constexpr void release_data() noexcept {
    delete[] data_;
  }

  constexpr void move_data(HString&& rhs) noexcept {
    data_ = rhs.data_;
    capacity_ = rhs.capacity_;
    size_ = rhs.size_;
    hash_ = rhs.hash_;

    rhs.data_ = nullptr;
    rhs.hash_ = kInitHash;
    rhs.size_ = 0;
    rhs.capacity_ = 0;
  }
};
// clang-format on


struct HStringHash {
  size_t operator()(const HString& s) const { return s.hash(); }
};


}  // namespace xylo

#endif  // XYLO_UTIL_HSTRING_H_
