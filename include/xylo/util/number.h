
#ifndef XYLO_UTIL_NUMBER_H_
#define XYLO_UTIL_NUMBER_H_

#include <charconv>
#include <cstdint>

namespace xylo {


enum class NumberKind {
  kInteger,
  kFloat,
};

struct NumberValue {
  NumberKind kind;
  union {
    int64_t integer_value;
    double float_value;
  } as;
};

struct ParseNumberResult {
  bool success;
  const char* ptr;
};


inline uint8_t CharToDigit(char c) {
  static constexpr uint8_t hexdigit[] = {
    // clang-format off
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
      0,   1,   2,   3,   4,   5,   6,   7,   8,   9, 255, 255, 255, 255, 255, 255,
    255, 10,   11,  12,  13,  14,  15, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 10,   11,  12,  13,  14,  15, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    // clang-format on
  };
  return hexdigit[static_cast<uint8_t>(c)];
}


inline auto ParseInteger(const char* input, int base, int max_digits, uint64_t* out_value) {
  auto p = input;
  uint64_t value = 0;
  int length = 0;

  while (length < max_digits) [[likely]] {
    auto digit = CharToDigit(*p);
    if (digit >= base) {
      break;
    }

    value = value * base + digit;
    ++p;
    ++length;
  }

  *out_value = value;
  return p;
}


inline ParseNumberResult ParseFloat(const char* first, const char* last, double* out_value) {
  auto result = std::from_chars(first, last, *out_value);
  if (result.ec == std::errc()) {
    return {true, result.ptr};
  } else {
    return {false, result.ptr};
  }
}


inline ParseNumberResult ParseNumber(const char* input, const char* nullpos, NumberValue* out_value) {
  auto p = input;

  bool negative = false;
  if (*p == '-') {
    negative = true;
    ++p;
  } else if (*p == '+') {
    ++p;
  }

  auto int_part_start = p;
  bool int_confirmed = false;
  int base = 10;
  int max_digits = 19;

  if (*p == '0') {
    ++p;

    switch (*p) {
      case 'x':
        int_confirmed = true;
        base = 16;
        max_digits = 16;
        ++p;
        int_part_start = p;
        break;

      case 'o':
        int_confirmed = true;
        base = 8;
        max_digits = 21;
        ++p;
        int_part_start = p;
        break;

      case 'b':
        int_confirmed = true;
        base = 2;
        max_digits = 64;
        ++p;
        int_part_start = p;
        break;

      default:
        break;
    }
  }

  uint64_t int_value;
  bool overflow = false;
  p = ParseInteger(p, base, max_digits, &int_value);

  if (int_value == 0100000000000000000000) {
    if (base == 8 && CharToDigit(*p) == 0) {
      int_value <<= 3;
      ++p;
    }
  }
  if (CharToDigit(*p) < base) {
    ++p;
    overflow = true;
    while (CharToDigit(*(p)) < base) ++p;
  }

  if (!int_confirmed && (*p == '.' || *p == 'e' || *p == 'E')) {
    out_value->kind = NumberKind::kFloat;
    return ParseFloat(input, nullpos, &(out_value->as.float_value));
  }

  out_value->kind = NumberKind::kInteger;

  if (p == int_part_start) {
    return {false, p};  // Invalid: no digits
  }
  if (overflow) {
    return {false, p};  // Invalid: integer overflow
  }

  if (negative) {
    if (int_value > static_cast<uint64_t>(INT64_MAX) + 1) {
      return {false, p};  // Invalid: integer overflow
    } else {
      out_value->as.integer_value = -static_cast<int64_t>(int_value);
      return {true, p};
    }
  } else {
    if (int_value > static_cast<uint64_t>(INT64_MAX)) {
      return {false, p};  // Invalid: integer overflow
    } else {
      out_value->as.integer_value = static_cast<int64_t>(int_value);
      return {true, p};
    }
  }
}


}  // namespace xylo

#endif  // XYLO_UTIL_NUMBER_H_
