
#ifndef XYLO_SYNTAX_TOKEN_H_
#define XYLO_SYNTAX_TOKEN_H_

#include "xylo/syntax/identifier.h"

namespace xylo {


enum class Token {
  kNull,
  kError,
  kEOF,
  kNewline,

  kInteger,
  kFloat,
  kChar,
  kString,
  kIdentifier,

  kAdd,
  kSub,
  kMul,
  kDiv,
  kRem,

  kAnd,
  kOr,
  kXor,
  kNot,
  kShl,
  kShr,

  kLAnd,
  kLOr,
  kLNot,

  kEQ,
  kNE,
  kLT,
  kLE,
  kGT,
  kGE,

  kAssign,
  kAddAssign,
  kSubAssign,
  kMulAssign,
  kDivAssign,
  kRemAssign,
  kAndAssign,
  kOrAssign,
  kXorAssign,
  kShlAssign,
  kShrAssign,

  kLParen,
  kRParen,
  kLBrack,
  kRBrack,
  kLBrace,
  kRBrace,

  kComma,
  kPeriod,
  kColon,
  kSemicolon,
  kQuestion,
  kArrow,
  kDoubleArrow,

  kInterface,
  kClass,
  kEmbed,
  kDef,
  kFn,
  kReturn,
  kLet,
  kVar,
  kIf,
  kElse,
  kNew,
  kTrue,
  kFalse,
  kThis,

  kTokenMax,
};


union TokenValue {
  const char* as_static_str;
  Identifier* as_identifier;
  int64_t as_integer;
  double as_float;
};


constexpr size_t TokenCount() {
  return static_cast<size_t>(Token::kTokenMax);
}


const char* TokenName(Token token);


struct TokenPrinter {
  Token token;
  const TokenValue* value;

  explicit TokenPrinter(Token t, const TokenValue* v = nullptr) :
      token(t),
      value(v) {}

  friend std::ostream& operator<<(std::ostream& os, const TokenPrinter& tp) {
    switch (tp.token) {
      case Token::kIdentifier:
        os << TokenName(tp.token);
        if (tp.value) {
          os << " '" << tp.value->as_identifier->str() << "'";
        }
        break;

      default:
        os << TokenName(tp.token);
        break;
    }
    return os;
  }
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_TOKEN_H_
