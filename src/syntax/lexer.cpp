
#include "xylo/syntax/lexer.h"

#include <limits>

#include "xylo/util/finally.h"
#include "xylo/util/hstring_view.h"
#include "xylo/util/number.h"

namespace xylo {


static constexpr HStringView kKeyword_Class("class");
static constexpr HStringView kKeyword_Embed("embed");
static constexpr HStringView kKeyword_Def("def");
static constexpr HStringView kKeyword_Fn("fn");
static constexpr HStringView kKeyword_Return("return");
static constexpr HStringView kKeyword_Let("let");
static constexpr HStringView kKeyword_Var("var");
static constexpr HStringView kKeyword_If("if");
static constexpr HStringView kKeyword_Else("else");
static constexpr HStringView kKeyword_New("new");
static constexpr HStringView kKeyword_True("true");
static constexpr HStringView kKeyword_False("false");
static constexpr HStringView kKeyword_This("this");


static bool IsStatementTail(Token token) {
  static bool* table = [] {
    static bool t[static_cast<size_t>(Token::kTokenMax)] = {};
    t[static_cast<size_t>(Token::kInteger)] = true;
    t[static_cast<size_t>(Token::kFloat)] = true;
    t[static_cast<size_t>(Token::kChar)] = true;
    t[static_cast<size_t>(Token::kString)] = true;
    t[static_cast<size_t>(Token::kIdentifier)] = true;
    t[static_cast<size_t>(Token::kTrue)] = true;
    t[static_cast<size_t>(Token::kFalse)] = true;
    t[static_cast<size_t>(Token::kReturn)] = true;

    t[static_cast<size_t>(Token::kRParen)] = true;
    t[static_cast<size_t>(Token::kRBrack)] = true;
    t[static_cast<size_t>(Token::kRBrace)] = true;
    return t;
  }();

  return table[static_cast<size_t>(token)];
}

static bool IsNonStatementHead(char c) {
  static bool* table = [] {
    static bool t[256] = {};
    auto ops = "+-*/%&|^<>=)]}.,:?";
    for (auto p = ops; *p; ++p) {
      t[static_cast<char8_t>(*p)] = true;
    }
    return t;
  }();

  return table[static_cast<char8_t>(c)];
}


static bool IsSpace(char c) {
  return (0x09 <= c && c <= 0x0d) || (c == 0x20);
}


static bool IsDigit(char c) {
  return ('0' <= c && c <= '9');
}


static bool IsIdentifierStart(char c) {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c == '_');
}


static bool IsIdentifierPart(char c) {
  return IsIdentifierStart(c) || IsDigit(c);
}


static Token CheckKeyword(const HStringView& ident_str) {
  switch (ident_str.hash()) {
    case kKeyword_Class.hash():
      if (ident_str == kKeyword_Class) {
        return Token::kClass;
      }
      break;

    case kKeyword_Embed.hash():
      if (ident_str == kKeyword_Embed) {
        return Token::kEmbed;
      }
      break;

    case kKeyword_Def.hash():
      if (ident_str == kKeyword_Def) {
        return Token::kDef;
      }
      break;

    case kKeyword_Fn.hash():
      if (ident_str == kKeyword_Fn) {
        return Token::kFn;
      }
      break;

    case kKeyword_Return.hash():
      if (ident_str == kKeyword_Return) {
        return Token::kReturn;
      }
      break;

    case kKeyword_Let.hash():
      if (ident_str == kKeyword_Let) {
        return Token::kLet;
      }
      break;

    case kKeyword_Var.hash():
      if (ident_str == kKeyword_Var) {
        return Token::kVar;
      }
      break;

    case kKeyword_If.hash():
      if (ident_str == kKeyword_If) {
        return Token::kIf;
      }
      break;

    case kKeyword_Else.hash():
      if (ident_str == kKeyword_Else) {
        return Token::kElse;
      }
      break;

    case kKeyword_New.hash():
      if (ident_str == kKeyword_New) {
        return Token::kNew;
      }
      break;

    case kKeyword_True.hash():
      if (ident_str == kKeyword_True) {
        return Token::kTrue;
      }
      break;

    case kKeyword_False.hash():
      if (ident_str == kKeyword_False) {
        return Token::kFalse;
      }
      break;

    case kKeyword_This.hash():
      if (ident_str == kKeyword_This) {
        return Token::kThis;
      }
      break;
  }

  return Token::kIdentifier;
}


Token Lexer::Next() {
  if (ghost_token_ != Token::kNull) {
    token_ = ghost_token_;
    value_ = ghost_value_;
    ghost_token_ = Token::kNull;
    return token_;
  }

  auto last_line = reader_.location().line;
  SkipSpacesAndComments();
  bool new_line = reader_.location().line != last_line;

  if (new_line) {
    if (IsStatementTail(last_token()) && !IsNonStatementHead(reader_.Peek())) {
      token_ = Token::kNewline;
      // end position of previous token
      position_.start = position_.end;
      position_.end = position_.end;
      return token_;
    }
  }

  // start and end position of the new token
  position_.start = reader_.location();
  Finally _([this]() {
    position_.end = reader_.location();
  });

  if (reader_.eof()) {
    token_ = Token::kEOF;
    return token_;
  }

  if ((token_ = TryReadNumber(!IsStatementTail(last_token()))) != Token::kNull) {
    return token_;
  }

  if ((token_ = TryReadIdentifier()) != Token::kNull) {
    return token_;
  }

  if ((token_ = TryReadOperator()) != Token::kNull) {
    return token_;
  }

  // unrecognized character
  token_ = Token::kError;
  reader_.Consume();
  set_value("unrecognized character");

  return token_;
}


void Lexer::SkipSpacesAndComments() {
  while (SkipSpaces() || SkipComments()) continue;
}


bool Lexer::SkipSpaces() {
  if (!IsSpace(reader_.Peek())) {
    return false;
  }

  do {
    reader_.Consume();
  } while (IsSpace(reader_.Peek()));

  return true;
}


bool Lexer::SkipComments() {
  // single-line comment
  if (reader_.TryConsume('#')) {
    while (!reader_.eof() && reader_.Peek() != '\n') {
      reader_.Consume();
    }
    return true;
  }

  return false;
}


Token Lexer::TryReadNumber(bool allow_sign_start) {
  auto digit_start = [this](size_t n) {
    return IsDigit(reader_.Peek(n));
  };
  auto point_start = [&, this](size_t n) {
    return reader_.Peek(n) == '.' && digit_start(n + 1);
  };
  auto sign_start = [&, this]() {
    return (reader_.Peek() == '+' || reader_.Peek() == '-') && (digit_start(1) || point_start(1));
  };

  if (!digit_start(0) && !point_start(0) && !(allow_sign_start && sign_start())) {
    return Token::kNull;
  }

  auto first = reader_.mark();
  auto last = reader_.last();

  NumberValue value;
  auto [success, next_ptr] = ParseNumber(first, last, &value);
  reader_.advance(next_ptr - first);

  if (value.kind == NumberKind::kInteger) {
    if (success) {
      set_value(value.as.integer_value);
      return Token::kInteger;
    } else {
      set_value("integer literal out of range");
      ghost_token_ = Token::kInteger;
      ghost_value_.as_integer = 0;
      return Token::kError;
    }
  } else {
    // value.kind == NumberKind::kFloat
    if (success) {
      set_value(value.as.float_value);
      return Token::kFloat;
    } else {
      set_value("float literal out of range");
      ghost_token_ = Token::kFloat;
      ghost_value_.as_float = 0.0;
      return Token::kError;
    }
  }
}


Token Lexer::TryReadIdentifier() {
  if (!IsIdentifierStart(reader_.Peek())) {
    return Token::kNull;
  }

  auto start = reader_.mark();
  do {
    reader_.Consume();
  } while (IsIdentifierPart(reader_.Peek()));
  auto end = reader_.mark();

  HStringView ident_str(start, end - start);

  Token token = CheckKeyword(ident_str);
  if (token != Token::kIdentifier) {
    return token;
  }

  auto identifier = context_->InternIdentifier(ident_str);
  set_value(identifier);
  return token;
}


Token Lexer::TryReadOperator() {
  switch (reader_.Peek()) {
    case '+':
      reader_.Consume();
      if (reader_.TryConsume('=')) {
        return Token::kAddAssign;
      } else {
        return Token::kAdd;
      }

    case '-':
      reader_.Consume();
      switch (reader_.Peek()) {
        case '=':
          reader_.Consume();
          return Token::kSubAssign;
        case '>':
          reader_.Consume();
          return Token::kArrow;
        default:
          return Token::kSub;
      }

    case '*':
      reader_.Consume();
      if (reader_.TryConsume('=')) {
        return Token::kMulAssign;
      } else {
        return Token::kMul;
      }

    case '/':
      reader_.Consume();
      if (reader_.TryConsume('=')) {
        return Token::kDivAssign;
      } else {
        return Token::kDiv;
      }

    case '%':
      reader_.Consume();
      if (reader_.TryConsume('=')) {
        return Token::kRemAssign;
      } else {
        return Token::kRem;
      }

    case '&':
      reader_.Consume();
      switch (reader_.Peek()) {
        case '&':
          reader_.Consume();
          return Token::kLAnd;
        case '=':
          reader_.Consume();
          return Token::kAndAssign;
        default:
          return Token::kAnd;
      }

    case '|':
      reader_.Consume();
      switch (reader_.Peek()) {
        case '|':
          reader_.Consume();
          return Token::kLOr;
        case '=':
          reader_.Consume();
          return Token::kOrAssign;
        default:
          return Token::kOr;
      }

    case '^':
      reader_.Consume();
      if (reader_.TryConsume('=')) {
        return Token::kXorAssign;
      } else {
        return Token::kXor;
      }

    case '~':
      reader_.Consume();
      return Token::kNot;

    case '<':
      reader_.Consume();
      switch (reader_.Peek()) {
        case '<':
          reader_.Consume();
          if (reader_.TryConsume('=')) {
            return Token::kShlAssign;
          } else {
            return Token::kShl;
          }
        case '=':
          reader_.Consume();
          return Token::kLE;
        default:
          return Token::kLT;
      }

    case '>':
      reader_.Consume();
      switch (reader_.Peek()) {
        case '>':
          reader_.Consume();
          if (reader_.TryConsume('=')) {
            return Token::kShrAssign;
          } else {
            return Token::kShr;
          }
        case '=':
          reader_.Consume();
          return Token::kGE;
        default:
          return Token::kGT;
      }

    case '!':
      reader_.Consume();
      if (reader_.TryConsume('=')) {
        return Token::kNE;
      } else {
        return Token::kLNot;
      }

    case '=':
      reader_.Consume();
      switch (reader_.Peek()) {
        case '=':
          reader_.Consume();
          return Token::kEQ;
        case '>':
          reader_.Consume();
          return Token::kDoubleArrow;
        default:
          return Token::kAssign;
      }

    case '(':
      reader_.Consume();
      return Token::kLParen;

    case ')':
      reader_.Consume();
      return Token::kRParen;

    case '[':
      reader_.Consume();
      return Token::kLBrack;

    case ']':
      reader_.Consume();
      return Token::kRBrack;

    case '{':
      reader_.Consume();
      return Token::kLBrace;

    case '}':
      reader_.Consume();
      return Token::kRBrace;

    case ',':
      reader_.Consume();
      return Token::kComma;

    case '.':
      reader_.Consume();
      return Token::kPeriod;

    case ':':
      reader_.Consume();
      return Token::kColon;

    case ';':
      reader_.Consume();
      return Token::kSemicolon;

    case '?':
      reader_.Consume();
      return Token::kQuestion;

    default:
      return Token::kNull;
  }
}


}  // namespace xylo
