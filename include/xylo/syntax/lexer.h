
#ifndef XYLO_SYNTAX_LEXER_H_
#define XYLO_SYNTAX_LEXER_H_

#include <cstddef>

#include "xylo/syntax/context.h"
#include "xylo/syntax/identifier.h"
#include "xylo/syntax/location.h"
#include "xylo/syntax/token.h"

namespace xylo {


class Reader {
 public:
  explicit Reader(const char* first, const char* last) :
      cursor_(first),
      last_(last),
      location_(0, 1, 1) {}

  ~Reader() = default;

  Reader(const Reader&) = delete;
  Reader& operator=(const Reader&) = delete;

  const char* last() const { return last_; }

  bool eof() const { return cursor_ == last_; }
  const char* mark() const { return cursor_; }

  // not intended for line breaks
  void advance(size_t n) {
    cursor_ += n;
    location_.offset += n;
    location_.column += n;
  }

  char Peek() const {
    auto c = *cursor_;
    // CR to LF. CRLF is handled in Consume().
    return (c == '\r') ? '\n' : c;
  }

  // no line ending conversion, no eof check.
  char Peek(size_t n) const { return *(cursor_ + n); }

  char Consume() {
    // CRLF to LF
    if (*cursor_ == '\r' && *(cursor_ + 1) == '\n') {
      cursor_++;
      location_.offset += 1;
    }

    auto c = *(cursor_++);
    location_.offset += 1;

    // CR to LF
    c = (c == '\r') ? '\n' : c;

    if (c == '\n') {
      location_.line += 1;
      location_.column = 1;
    } else {
      location_.column += 1;
    }

    return c;
  }

  bool TryConsume(char expected) {
    if (Peek() != expected) {
      return false;
    }
    Consume();
    return true;
  }

  const SourceLocation& location() const { return location_; }

 private:
  const char* cursor_;
  const char* last_;
  SourceLocation location_;
};


class Lexer {
 public:
  Lexer(XyloContext* context, SourceFile* file) :
      context_(context),
      reader_(file->content(), file->content() + file->size()),
      token_(Token::kNull),
      value_(),
      position_(file),
      ghost_token_(Token::kNull),
      ghost_value_() {}

  Lexer(XyloContext* context, const char* input) :
      context_(context),
      reader_(input, input + std::strlen(input)),
      token_(Token::kNull),
      value_(),
      position_(),
      ghost_token_(Token::kNull),
      ghost_value_() {}

  ~Lexer() = default;

  Lexer(const Lexer&) = delete;
  Lexer& operator=(const Lexer&) = delete;

  Token Next();

  Token last_token() const { return token_; }
  const TokenValue& last_value() const { return value_; }
  const SourceRange& last_position() const { return position_; }

 protected:
  void SkipSpacesAndComments();
  bool SkipSpaces();
  bool SkipComments();

  Token TryReadNumber(bool allow_sign_start);
  Token TryReadIdentifier();
  Token TryReadOperator();

  void set_value(const char* static_str) { value_.as_static_str = static_str; }
  void set_value(Identifier* ident) { value_.as_identifier = ident; }
  void set_value(int64_t int_value) { value_.as_integer = int_value; }
  void set_value(double float_value) { value_.as_float = float_value; }

 private:
  XyloContext* context_;
  Reader reader_;

  Token token_;
  TokenValue value_;
  SourceRange position_;

  Token ghost_token_;
  TokenValue ghost_value_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_LEXER_H_
