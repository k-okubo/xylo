
#ifndef XYLO_UTIL_STRING_H_
#define XYLO_UTIL_STRING_H_

#include <cstring>
#include <iostream>
#include <string>
#include <utility>

#include "xylo/util/hstring.h"

namespace xylo {


// clang-format off
class String {
 public:
  using charT = char;
  using size_type = size_t;
  static_assert(sizeof(charT) == 1);

  constexpr String() noexcept : data_(nullptr), capacity_(0), size_(0) {}

  constexpr explicit String(const charT* s) : String(s, std::char_traits<charT>::length(s)) {}

  constexpr String(const charT* s, size_type n) : capacity_(n), size_(n) {
    data_ = new charT[n];
    std::memcpy(data_, s, n);
  }

  constexpr ~String() {
    release_data();
  }

  String(const String&) = delete;
  String& operator=(const String&) = delete;

  constexpr String(String&& rhs) noexcept {
    move_data(std::move(rhs));
  }

  constexpr String& operator=(String&& rhs) noexcept {
    release_data();
    move_data(std::move(rhs));
    return *this;
  }

  constexpr size_type size() const noexcept { return size_; }
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
  }

  constexpr String& append(const charT* s, size_type n) {
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

    std::memcpy(data_ + size_, s, n);
    size_ += n;

    return *this;
  }

  friend constexpr bool operator==(const String& lhs, const String& rhs) noexcept {
    return lhs.size_ == rhs.size_ && std::memcmp(lhs.data_, rhs.data_, lhs.size_) == 0;
  }

  friend constexpr bool operator!=(const String& lhs, const String& rhs) noexcept {
    return !(lhs == rhs);
  }

  friend std::ostream& operator<<(std::ostream& stream, const String& string) {
    return stream.write(string.data_, string.size_);
  }

  String& operator+=(const String& rhs) {
    return append(rhs.data_, rhs.size_);
  }

  String& operator+=(const HString& rhs) {
    return append(rhs.data(), rhs.size());
  }

  String& operator+=(const charT* rhs) {
    return append(rhs, std::char_traits<charT>::length(rhs));
  }

  String& operator+=(const std::string& rhs) {
    return append(rhs.data(), rhs.size());
  }

 private:
  static constexpr size_type kInitCapacity = 16;

  charT* data_;
  size_type capacity_;
  size_type size_;

  constexpr void release_data() noexcept {
    delete[] data_;
  }

  constexpr void move_data(String&& rhs) noexcept {
    data_ = rhs.data_;
    capacity_ = rhs.capacity_;
    size_ = rhs.size_;

    rhs.data_ = nullptr;
    rhs.size_ = 0;
    rhs.capacity_ = 0;
  }
};
// clang-format on


}  // namespace xylo

#endif  // XYLO_UTIL_STRING_H_
