
#include "xylo/syntax/lexer.h"

#include <gtest/gtest.h>

namespace xylo {

static_assert(!std::copyable<Lexer>);
static_assert(!std::movable<Lexer>);
static_assert(!std::copyable<Reader>);
static_assert(!std::movable<Reader>);


TEST(LexerTest, BufferReading) {
  auto input = "ab\n12\r\nXY\r+-";
  Reader reader(input, input + std::strlen(input));

  EXPECT_EQ(reader.location().offset, 0u);
  EXPECT_EQ(reader.location().line, 1u);
  EXPECT_EQ(reader.location().column, 1u);

  EXPECT_EQ(reader.Peek(), 'a');
  EXPECT_EQ(reader.location().offset, 0u);
  EXPECT_EQ(reader.location().line, 1u);
  EXPECT_EQ(reader.location().column, 1u);
  EXPECT_EQ(reader.Consume(), 'a');

  EXPECT_EQ(reader.Peek(), 'b');
  EXPECT_EQ(reader.location().offset, 1u);
  EXPECT_EQ(reader.location().line, 1u);
  EXPECT_EQ(reader.location().column, 2u);
  EXPECT_EQ(reader.Consume(), 'b');

  EXPECT_EQ(reader.Peek(), '\n');
  EXPECT_EQ(reader.location().offset, 2u);
  EXPECT_EQ(reader.location().line, 1u);
  EXPECT_EQ(reader.location().column, 3u);
  EXPECT_EQ(reader.Consume(), '\n');

  EXPECT_EQ(reader.Peek(), '1');
  EXPECT_EQ(reader.location().offset, 3u);
  EXPECT_EQ(reader.location().line, 2u);
  EXPECT_EQ(reader.location().column, 1u);
  EXPECT_EQ(reader.Consume(), '1');

  EXPECT_EQ(reader.Peek(), '2');
  EXPECT_EQ(reader.location().offset, 4u);
  EXPECT_EQ(reader.location().line, 2u);
  EXPECT_EQ(reader.location().column, 2u);
  EXPECT_EQ(reader.Consume(), '2');

  EXPECT_EQ(reader.Peek(), '\n');
  EXPECT_EQ(reader.location().offset, 5u);
  EXPECT_EQ(reader.location().line, 2u);
  EXPECT_EQ(reader.location().column, 3u);
  EXPECT_EQ(reader.Consume(), '\n');

  EXPECT_EQ(reader.Peek(), 'X');
  EXPECT_EQ(reader.location().offset, 7u);
  EXPECT_EQ(reader.location().line, 3u);
  EXPECT_EQ(reader.location().column, 1u);
  EXPECT_EQ(reader.Consume(), 'X');

  EXPECT_EQ(reader.Peek(), 'Y');
  EXPECT_EQ(reader.location().offset, 8u);
  EXPECT_EQ(reader.location().line, 3u);
  EXPECT_EQ(reader.location().column, 2u);
  EXPECT_EQ(reader.Consume(), 'Y');

  EXPECT_EQ(reader.Peek(), '\n');
  EXPECT_EQ(reader.location().offset, 9u);
  EXPECT_EQ(reader.location().line, 3u);
  EXPECT_EQ(reader.location().column, 3u);
  EXPECT_EQ(reader.Consume(), '\n');

  EXPECT_EQ(reader.Peek(), '+');
  EXPECT_EQ(reader.location().offset, 10u);
  EXPECT_EQ(reader.location().line, 4u);
  EXPECT_EQ(reader.location().column, 1u);
  EXPECT_EQ(reader.Consume(), '+');

  EXPECT_EQ(reader.Peek(), '-');
  EXPECT_EQ(reader.location().offset, 11u);
  EXPECT_EQ(reader.location().line, 4u);
  EXPECT_EQ(reader.location().column, 2u);
  EXPECT_EQ(reader.Consume(), '-');

  EXPECT_TRUE(reader.eof());
  EXPECT_EQ(reader.location().offset, 12u);
  EXPECT_EQ(reader.location().line, 4u);
  EXPECT_EQ(reader.location().column, 3u);
}


TEST(LexerTest, EmptyInput) {
  auto input = "";
  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kEOF);
  EXPECT_EQ(lexer.last_position().start.offset, 0u);
  EXPECT_EQ(lexer.last_position().start.line, 1u);
  EXPECT_EQ(lexer.last_position().start.column, 1u);
  EXPECT_EQ(lexer.last_position().end.offset, 0u);
  EXPECT_EQ(lexer.last_position().end.line, 1u);
  EXPECT_EQ(lexer.last_position().end.column, 1u);
}


TEST(LexerTest, OnlySpaces) {
  auto input = "    \n\t  \r\n ";
  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kEOF);
  EXPECT_EQ(lexer.last_position().start.offset, 11u);
  EXPECT_EQ(lexer.last_position().start.line, 3u);
  EXPECT_EQ(lexer.last_position().start.column, 2u);
  EXPECT_EQ(lexer.last_position().end.offset, 11u);
  EXPECT_EQ(lexer.last_position().end.line, 3u);
  EXPECT_EQ(lexer.last_position().end.column, 2u);
}


TEST(LexerTest, Basic) {
  auto input = "def foo(x) = x + 42";
  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kDef);
  EXPECT_EQ(lexer.last_position().start.offset, 0u);
  EXPECT_EQ(lexer.last_position().start.line, 1u);
  EXPECT_EQ(lexer.last_position().start.column, 1u);
  EXPECT_EQ(lexer.last_position().end.offset, 3u);
  EXPECT_EQ(lexer.last_position().end.line, 1u);
  EXPECT_EQ(lexer.last_position().end.column, 4u);

  EXPECT_EQ(lexer.Next(), Token::kIdentifier);
  EXPECT_EQ(lexer.last_value().as_identifier->str().cpp_str(), "foo");
  EXPECT_EQ(lexer.last_position().start.offset, 4u);
  EXPECT_EQ(lexer.last_position().start.line, 1u);
  EXPECT_EQ(lexer.last_position().start.column, 5u);
  EXPECT_EQ(lexer.last_position().end.offset, 7u);
  EXPECT_EQ(lexer.last_position().end.line, 1u);
  EXPECT_EQ(lexer.last_position().end.column, 8u);

  EXPECT_EQ(lexer.Next(), Token::kLParen);
  EXPECT_EQ(lexer.last_position().start.offset, 7u);
  EXPECT_EQ(lexer.last_position().start.line, 1u);
  EXPECT_EQ(lexer.last_position().start.column, 8u);
  EXPECT_EQ(lexer.last_position().end.offset, 8u);
  EXPECT_EQ(lexer.last_position().end.line, 1u);
  EXPECT_EQ(lexer.last_position().end.column, 9u);

  EXPECT_EQ(lexer.Next(), Token::kIdentifier);
  EXPECT_EQ(lexer.last_value().as_identifier->str().cpp_str(), "x");
  EXPECT_EQ(lexer.last_position().start.offset, 8u);
  EXPECT_EQ(lexer.last_position().start.line, 1u);
  EXPECT_EQ(lexer.last_position().start.column, 9u);
  EXPECT_EQ(lexer.last_position().end.offset, 9u);
  EXPECT_EQ(lexer.last_position().end.line, 1u);
  EXPECT_EQ(lexer.last_position().end.column, 10u);

  EXPECT_EQ(lexer.Next(), Token::kRParen);
  EXPECT_EQ(lexer.last_position().start.offset, 9u);
  EXPECT_EQ(lexer.last_position().start.line, 1u);
  EXPECT_EQ(lexer.last_position().start.column, 10u);
  EXPECT_EQ(lexer.last_position().end.offset, 10u);
  EXPECT_EQ(lexer.last_position().end.line, 1u);
  EXPECT_EQ(lexer.last_position().end.column, 11u);

  EXPECT_EQ(lexer.Next(), Token::kAssign);
  EXPECT_EQ(lexer.last_position().start.offset, 11u);
  EXPECT_EQ(lexer.last_position().start.line, 1u);
  EXPECT_EQ(lexer.last_position().start.column, 12u);
  EXPECT_EQ(lexer.last_position().end.offset, 12u);
  EXPECT_EQ(lexer.last_position().end.line, 1u);
  EXPECT_EQ(lexer.last_position().end.column, 13u);

  EXPECT_EQ(lexer.Next(), Token::kIdentifier);
  EXPECT_EQ(lexer.last_value().as_identifier->str().cpp_str(), "x");
  EXPECT_EQ(lexer.last_position().start.offset, 13u);
  EXPECT_EQ(lexer.last_position().start.line, 1u);
  EXPECT_EQ(lexer.last_position().start.column, 14u);
  EXPECT_EQ(lexer.last_position().end.offset, 14u);
  EXPECT_EQ(lexer.last_position().end.line, 1u);
  EXPECT_EQ(lexer.last_position().end.column, 15u);

  EXPECT_EQ(lexer.Next(), Token::kAdd);
  EXPECT_EQ(lexer.last_position().start.offset, 15u);
  EXPECT_EQ(lexer.last_position().start.line, 1u);
  EXPECT_EQ(lexer.last_position().start.column, 16u);
  EXPECT_EQ(lexer.last_position().end.offset, 16u);
  EXPECT_EQ(lexer.last_position().end.line, 1u);
  EXPECT_EQ(lexer.last_position().end.column, 17u);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, 42);
  EXPECT_EQ(lexer.last_position().start.offset, 17u);
  EXPECT_EQ(lexer.last_position().start.line, 1u);
  EXPECT_EQ(lexer.last_position().start.column, 18u);
  EXPECT_EQ(lexer.last_position().end.offset, 19u);
  EXPECT_EQ(lexer.last_position().end.line, 1u);
  EXPECT_EQ(lexer.last_position().end.column, 20u);

  EXPECT_EQ(lexer.Next(), Token::kEOF);
  EXPECT_EQ(lexer.last_position().start.offset, 19u);
  EXPECT_EQ(lexer.last_position().start.line, 1u);
  EXPECT_EQ(lexer.last_position().start.column, 20u);
  EXPECT_EQ(lexer.last_position().end.offset, 19u);
  EXPECT_EQ(lexer.last_position().end.line, 1u);
  EXPECT_EQ(lexer.last_position().end.column, 20u);
}


TEST(LexerTest, CommentsAndSpaces) {
  auto input = "  # this is a comment\n\tdef foo # another comment\n(x) # one more\n= x + 42 # end";
  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kDef);
  EXPECT_EQ(lexer.Next(), Token::kIdentifier);
  EXPECT_EQ(lexer.last_value().as_identifier->str().cpp_str(), "foo");
  EXPECT_EQ(lexer.Next(), Token::kNewline);
  EXPECT_EQ(lexer.Next(), Token::kLParen);
  EXPECT_EQ(lexer.Next(), Token::kIdentifier);
  EXPECT_EQ(lexer.last_value().as_identifier->str().cpp_str(), "x");
  EXPECT_EQ(lexer.Next(), Token::kRParen);
  EXPECT_EQ(lexer.Next(), Token::kAssign);
  EXPECT_EQ(lexer.Next(), Token::kIdentifier);
  EXPECT_EQ(lexer.last_value().as_identifier->str().cpp_str(), "x");
  EXPECT_EQ(lexer.Next(), Token::kAdd);
  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, 42);
  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


TEST(LexerTest, InvalidInput) {
  auto input = "@";
  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kError);
  EXPECT_EQ(lexer.last_position().start.offset, 0u);
  EXPECT_EQ(lexer.last_position().start.line, 1u);
  EXPECT_EQ(lexer.last_position().start.column, 1u);
  EXPECT_EQ(lexer.last_position().end.offset, 1u);
  EXPECT_EQ(lexer.last_position().end.line, 1u);
  EXPECT_EQ(lexer.last_position().end.column, 2u);
  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


TEST(LexerTest, DecimalIntegerNumber) {
  auto input = "0, +0, -0, 1, -1";
  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, 0);
  EXPECT_EQ(lexer.last_position().start.offset, 0u);
  EXPECT_EQ(lexer.last_position().end.offset, 1u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, 0);
  EXPECT_EQ(lexer.last_position().start.offset, 3u);
  EXPECT_EQ(lexer.last_position().end.offset, 5u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, 0);
  EXPECT_EQ(lexer.last_position().start.offset, 7u);
  EXPECT_EQ(lexer.last_position().end.offset, 9u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, 1);
  EXPECT_EQ(lexer.last_position().start.offset, 11u);
  EXPECT_EQ(lexer.last_position().end.offset, 12u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, -1);
  EXPECT_EQ(lexer.last_position().start.offset, 14u);
  EXPECT_EQ(lexer.last_position().end.offset, 16u);
  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


TEST(LexerTest, DecimalIntegerNumber_TooLarge) {
  auto input = "123456789012345678901234567890";
  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kError);
  EXPECT_STREQ(lexer.last_value().as_static_str, "integer literal out of range");
  EXPECT_EQ(lexer.last_position().start.offset, 0u);
  EXPECT_EQ(lexer.last_position().end.offset, 30u);
  EXPECT_EQ(lexer.Next(), Token::kInteger);  // ghost token for recovery
  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


TEST(LexerTest, ArithmeticOrSign) {
  auto input = "1+2, a-3";
  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, 1);
  EXPECT_EQ(lexer.Next(), Token::kAdd);
  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, 2);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kIdentifier);
  EXPECT_EQ(lexer.last_value().as_identifier->str().cpp_str(), "a");
  EXPECT_EQ(lexer.Next(), Token::kSub);
  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, 3);
  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


TEST(LexerTest, DecimalIntegerNumber_Limits) {
  auto input =
      "9223372036854775807,"
      "9223372036854775808,"
      "-9223372036854775808,"
      "-9223372036854775809";

  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, 9223372036854775807);
  EXPECT_EQ(lexer.last_position().start.offset, 0u);
  EXPECT_EQ(lexer.last_position().end.offset, 19u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kError);
  EXPECT_STREQ(lexer.last_value().as_static_str, "integer literal out of range");
  EXPECT_EQ(lexer.last_position().start.offset, 20u);
  EXPECT_EQ(lexer.last_position().end.offset, 39u);

  EXPECT_EQ(lexer.Next(), Token::kInteger);  // ghost token for recovery
  EXPECT_EQ(lexer.last_value().as_integer, 0);
  EXPECT_EQ(lexer.last_position().start.offset, 20u);
  EXPECT_EQ(lexer.last_position().end.offset, 39u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, -9223372036854775807 - 1);
  EXPECT_EQ(lexer.last_position().start.offset, 40u);
  EXPECT_EQ(lexer.last_position().end.offset, 60u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kError);
  EXPECT_STREQ(lexer.last_value().as_static_str, "integer literal out of range");
  EXPECT_EQ(lexer.last_position().start.offset, 61u);
  EXPECT_EQ(lexer.last_position().end.offset, 81u);

  EXPECT_EQ(lexer.Next(), Token::kInteger);  // ghost token for recovery
  EXPECT_EQ(lexer.last_value().as_integer, 0);
  EXPECT_EQ(lexer.last_position().start.offset, 61u);
  EXPECT_EQ(lexer.last_position().end.offset, 81u);
  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


TEST(LexerTest, OctalIntegerNumber_Limits) {
  auto input =
      "0o777777777777777777777,"
      "0o1000000000000000000000,"
      "-0o1000000000000000000000,"
      "-0o1000000000000000000001,"
      "-0o2000000000000000000000";

  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, 0777777777777777777777);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kError);
  EXPECT_STREQ(lexer.last_value().as_static_str, "integer literal out of range");
  EXPECT_EQ(lexer.Next(), Token::kInteger);  // ghost token
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, -01000000000000000000000);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kError);
  EXPECT_STREQ(lexer.last_value().as_static_str, "integer literal out of range");
  EXPECT_EQ(lexer.Next(), Token::kInteger);  // ghost token
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kError);
  EXPECT_STREQ(lexer.last_value().as_static_str, "integer literal out of range");
  EXPECT_EQ(lexer.Next(), Token::kInteger);  // ghost token
  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


TEST(LexerTest, HexadecimalIntegerNumber_Limits) {
  auto input =
      "0x7FFFFFFFFFFFFFFF,"
      "0x8000000000000000,"
      "-0x8000000000000000,"
      "-0x8000000000000001";

  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, 0x7FFFFFFFFFFFFFFF);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kError);
  EXPECT_STREQ(lexer.last_value().as_static_str, "integer literal out of range");
  EXPECT_EQ(lexer.Next(), Token::kInteger);  // ghost token for recovery
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kInteger);
  EXPECT_EQ(lexer.last_value().as_integer, -0x8000000000000000);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kError);
  EXPECT_STREQ(lexer.last_value().as_static_str, "integer literal out of range");
  EXPECT_EQ(lexer.Next(), Token::kInteger);  // ghost token for recovery
  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


TEST(LexerTest, FloatNumber) {
  auto input = "0.0, -0.0, .1, -.1, 1., -1., 1e10, -1E10, 1.5e-10, -1.5E+10";
  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, 0.0);
  EXPECT_EQ(lexer.last_position().start.offset, 0u);
  EXPECT_EQ(lexer.last_position().end.offset, 3u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, -0.0);
  EXPECT_EQ(lexer.last_position().start.offset, 5u);
  EXPECT_EQ(lexer.last_position().end.offset, 9u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, .1);
  EXPECT_EQ(lexer.last_position().start.offset, 11u);
  EXPECT_EQ(lexer.last_position().end.offset, 13u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, -.1);
  EXPECT_EQ(lexer.last_position().start.offset, 15u);
  EXPECT_EQ(lexer.last_position().end.offset, 18u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, 1.);
  EXPECT_EQ(lexer.last_position().start.offset, 20u);
  EXPECT_EQ(lexer.last_position().end.offset, 22u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, -1.);
  EXPECT_EQ(lexer.last_position().start.offset, 24u);
  EXPECT_EQ(lexer.last_position().end.offset, 27u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, 1e10);
  EXPECT_EQ(lexer.last_position().start.offset, 29u);
  EXPECT_EQ(lexer.last_position().end.offset, 33u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, -1E10);
  EXPECT_EQ(lexer.last_position().start.offset, 35u);
  EXPECT_EQ(lexer.last_position().end.offset, 40u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, 1.5e-10);
  EXPECT_EQ(lexer.last_position().start.offset, 42u);
  EXPECT_EQ(lexer.last_position().end.offset, 49u);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, -1.5E+10);
  EXPECT_EQ(lexer.last_position().start.offset, 51u);
  EXPECT_EQ(lexer.last_position().end.offset, 59u);
  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


TEST(LexerTest, FloatNumber_Contrived) {
  auto input = "1.2345e22, 9007199254740992.0, 9007199254740993.0, 9007199254740994.0, 9007199254740995.0,";

  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, 1.2345e22);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, 9007199254740992.0);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, 9007199254740993.0);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, 9007199254740994.0);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  EXPECT_EQ(lexer.last_value().as_float, 9007199254740995.0);
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


TEST(LexerTest, FloatNumber_LargeLimit) {
  auto input =
      "1."
      "7976931348623158079372897140530341507993413271003782693617377898044496829276475094664901797758720709633028641669"
      "2887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622"
      "8429148198608349364752927190741684443655107043427115596995080930428801779041744977919999999999e308,"
      "1."
      "7976931348623158079372897140530341507993413271003782693617377898044496829276475094664901797758720709633028641669"
      "2887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622"
      "842914819860834936475292719074168444365510704342711559699508093042880177904174497792e308,";

  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  double value = lexer.last_value().as_float;
  uint64_t bits = std::bit_cast<uint64_t>(value);
  EXPECT_EQ(bits, 0x7fefffffffffffff);  // max finite double
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kError);
  EXPECT_STREQ(lexer.last_value().as_static_str, "float literal out of range");
  EXPECT_EQ(lexer.Next(), Token::kFloat);  // ghost token
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


TEST(LexerTest, FloatNumber_NormalLimit) {
  auto input =
      "2."
      "2250738585072011360574097967091319759348195463516456480234261097248222220210769455165295239081350879141491589130"
      "3962110687008643869459464552765720740782062174337998814106326732925355228688137214901298112245145188984905722230"
      "7285255133155755015914397476397983411801999323962548289017107081850690630666655994938275772572015763062690663332"
      "6475653000092458883164330377797918696120494973903778297049050510806099407302629371289589500035837999672072543043"
      "6028407889577179615094551674824347103070260914462157228988025818254518032570701886087211312807951223342628836862"
      "2321503775666622503982534335974568884423900265498198385487948292206894721689831099698365846814022854243330660339"
      "85088644580400103493397042756718644338377048603786162277173854562306587467901408672332763671875e-308,"
      "2."
      "2250738585072011360574097967091319759348195463516456480234261097248222220210769455165295239081350879141491589130"
      "3962110687008643869459464552765720740782062174337998814106326732925355228688137214901298112245145188984905722230"
      "7285255133155755015914397476397983411801999323962548289017107081850690630666655994938275772572015763062690663332"
      "6475653000092458883164330377797918696120494973903778297049050510806099407302629371289589500035837999672072543043"
      "6028407889577179615094551674824347103070260914462157228988025818254518032570701886087211312807951223342628836862"
      "2321503775666622503982534335974568884423900265498198385487948292206894721689831099698365846814022854243330660339"
      "850886445804001034933970427567186443383770486037861622771738545623065874679014086723327636718749999999999e-308,";


  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  double value1 = lexer.last_value().as_float;
  uint64_t bits1 = std::bit_cast<uint64_t>(value1);
  EXPECT_EQ(bits1, 0x0010000000000000);  // min normal double
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  double value2 = lexer.last_value().as_float;
  uint64_t bits2 = std::bit_cast<uint64_t>(value2);
  EXPECT_EQ(bits2, 0x000fffffffffffff);  // max subnormal double
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


TEST(LexerTest, FloatNumber_SmallLimit) {
  auto input =
      "2."
      "4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772"
      "2858865463328355177969898199387398005390939063150356595155702263922908583924491051844359318028499365361525003193"
      "7045767824921936562366986365848075700158576926990370631192827955855133292783433840935197801553124659726357957462"
      "2766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968"
      "9513405355374585166611342237666786041621596804619144672918403005300575308490487653917113865916462395249126236538"
      "8187963623937328042389101867234849766823508986338858792562830275599565752445550725518931369083625477918694866799"
      "49683240497058210285131854513962138377228261454376934125320985913276672363281250000000001e-324,"
      "2."
      "4703282292062327208828439643411068618252990130716238221279284125033775363510437593264991818081799618989828234772"
      "2858865463328355177969898199387398005390939063150356595155702263922908583924491051844359318028499365361525003193"
      "7045767824921936562366986365848075700158576926990370631192827955855133292783433840935197801553124659726357957462"
      "2766465272827220056374006485499977096599470454020828166226237857393450736339007967761930577506740176324673600968"
      "9513405355374585166611342237666786041621596804619144672918403005300575308490487653917113865916462395249126236538"
      "8187963623937328042389101867234849766823508986338858792562830275599565752445550725518931369083625477918694866799"
      "4968324049705821028513185451396213837722826145437693412532098591327667236328125e-324,";

  XyloContext context;
  Lexer lexer(&context, input);

  EXPECT_EQ(lexer.Next(), Token::kFloat);
  double value = lexer.last_value().as_float;
  uint64_t bits = std::bit_cast<uint64_t>(value);
  EXPECT_EQ(bits, 0x0000000000000001);  // min subnormal double
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kError);
  EXPECT_STREQ(lexer.last_value().as_static_str, "float literal out of range");
  EXPECT_EQ(lexer.Next(), Token::kFloat);  // ghost token
  EXPECT_EQ(lexer.Next(), Token::kComma);

  EXPECT_EQ(lexer.Next(), Token::kEOF);
}


}  // namespace xylo
