
#ifndef XYLO_SYNTAX_PARSER_H_
#define XYLO_SYNTAX_PARSER_H_

#include <initializer_list>

#include "xylo/syntax/ast.h"
#include "xylo/syntax/context.h"
#include "xylo/syntax/diagnostic.h"
#include "xylo/syntax/lexer.h"


namespace xylo {


class Parser : public DiagnosticReporter {
 public:
  explicit Parser(XyloContext* context, Lexer* lexer) :
      DiagnosticReporter(),
      context_(context),
      lexer_(lexer),
      last_token_value_(),
      last_token_position_() {
    lexer_->Next();
  }

  ~Parser() = default;

  Parser(const Parser&) = delete;
  Parser& operator=(const Parser&) = delete;

  FileASTPtr ParseFile();

  DeclarationPtr ParseInterfaceDeclaration(Scope* scope);
  DeclarationPtr ParseClassDeclaration(Scope* scope);
  ClassFieldPtr ParseClassField(Scope* scope);
  EmbeddingClassPtr ParseEmbeddingClass(Scope* scope);
  Vector<SuperClassPtr> ParseSuperClasses(Scope* scope);

  FunctionDeclarationPtr ParseFunctionDeclaration(Scope* scope, bool needs_body = true);
  FunctionExpressionPtr ParseLambdaExpression(Scope* scope, bool needs_body = true);
  BlockPtr ParseBlock(Scope* scope);

  StatementPtr ParseExpressionStatement(Scope* scope);
  StatementPtr ParseLetStatement(Scope* scope);
  StatementPtr ParseVarStatement(Scope* scope);
  StatementPtr ParseReturnStatement(Scope* scope);

  TupleExpressionPtr ParseTupleExpression(Scope* scope);
  ExpressionPtr ParseExpression(Scope* scope, int min_precedence = 0);
  ExpressionPtr ParseTernaryExpression(Scope* scope, int min_precedence);
  ExpressionPtr ParseBinaryExpression(Scope* scope, int min_precedence);
  ExpressionPtr ParseUnaryExpression(Scope* scope, int min_precedence);
  ExpressionPtr ParsePrefixExpression(Scope* scope);
  ExpressionPtr ParsePrimaryExpression(Scope* scope);
  ExpressionPtr ParseConditionalExpression(Scope* scope);
  ExpressionPtr ParseNewExpression(Scope* scope);

  ObjectInitializerPtr ParseObjectInitializer(Scope* scope);
  InitializerPtr ParseExpressionInitializer(Scope* scope);

  TypeReprPtr ParseTypeRepr();
  TypeReprPtr ParseFunctionTypeRepr();
  TypeReprPtr ParsePrimaryTypeRepr();
  TypeReprPtr ParseTupleTypeRepr();

 protected:
  void ErrorExpected(Token expected);
  void ErrorExpected(Token expected1, Token expected2);
  void ErrorUnexpected();
  void Synchronize(std::initializer_list<Token> sync);

  Token PeekAhead() const { return lexer_->last_token(); }
  const TokenValue& PeekTokenValue() const { return lexer_->last_value(); }
  const SourceRange& PeekTokenPosition() const { return lexer_->last_position(); }

  const TokenValue& LastTokenValue() const { return last_token_value_; }
  const SourceRange& LastTokenPosition() const { return last_token_position_; }

  Token Consume() {
    auto last_token = PeekAhead();
    last_token_value_ = PeekTokenValue();
    last_token_position_ = PeekTokenPosition();
    lexer_->Next();  // pre-read token

    if (PeekAhead() == Token::kError) {
      ReportError(PeekTokenPosition(), PeekTokenValue().as_static_str);
      while ((lexer_->Next()) == Token::kError) continue;
    }

    return last_token;
  }

  bool TryConsume(Token token) {
    if (PeekAhead() == token) {
      Consume();
      return true;
    } else {
      return false;
    }
  }

  bool Expect(Token token) {
    if (TryConsume(token)) {
      return true;
    } else {
      ErrorExpected(token);
      return false;
    }
  }

  bool TryConsumeEndOfStatement() {
    if (TryConsume(Token::kSemicolon) || TryConsume(Token::kNewline)) {
      return true;
    } else if (PeekAhead() == Token::kRBrace || PeekAhead() == Token::kEOF) {
      return true;
    } else {
      return false;
    }
  }

  bool ExpectEndOfStatement() {
    if (TryConsumeEndOfStatement()) {
      return true;
    } else {
      ErrorExpected(Token::kSemicolon, Token::kNewline);
      return false;
    }
  }

 private:
  XyloContext* context_;
  Lexer* lexer_;

  TokenValue last_token_value_;
  SourceRange last_token_position_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_PARSER_H_
