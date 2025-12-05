
#include "xylo/syntax/parser.h"

#include <algorithm>
#include <sstream>

namespace xylo {


enum class Associativity {
  kLeft,
  kRight,
};


static Associativity GetAssociativity(Token token) {
  static Associativity* table = [] {
    static Associativity t[TokenCount()];
    for (size_t i = 0; i < TokenCount(); ++i) {
      t[i] = Associativity::kLeft;
    }
    t[static_cast<size_t>(Token::kAssign)] = Associativity::kRight;
    t[static_cast<size_t>(Token::kAddAssign)] = Associativity::kRight;
    t[static_cast<size_t>(Token::kSubAssign)] = Associativity::kRight;
    t[static_cast<size_t>(Token::kMulAssign)] = Associativity::kRight;
    t[static_cast<size_t>(Token::kDivAssign)] = Associativity::kRight;
    t[static_cast<size_t>(Token::kRemAssign)] = Associativity::kRight;
    t[static_cast<size_t>(Token::kAndAssign)] = Associativity::kRight;
    t[static_cast<size_t>(Token::kOrAssign)] = Associativity::kRight;
    t[static_cast<size_t>(Token::kXorAssign)] = Associativity::kRight;
    t[static_cast<size_t>(Token::kShlAssign)] = Associativity::kRight;
    t[static_cast<size_t>(Token::kShrAssign)] = Associativity::kRight;
    return t;
  }();

  return table[static_cast<size_t>(token)];
}


static int PrefixPrecedence(Token token) {
  static int* table = [] {
    static int t[TokenCount()];
    for (size_t i = 0; i < TokenCount(); ++i) {
      t[i] = -1;
    }
    t[static_cast<size_t>(Token::kAdd)] = 140;
    t[static_cast<size_t>(Token::kSub)] = 140;
    t[static_cast<size_t>(Token::kNot)] = 140;
    t[static_cast<size_t>(Token::kLNot)] = 140;
    return t;
  }();

  return table[static_cast<size_t>(token)];
}


static int PostfixPrecedence(Token token) {
  static int* table = [] {
    static int t[TokenCount()];
    for (size_t i = 0; i < TokenCount(); ++i) {
      t[i] = -1;
    }
    t[static_cast<size_t>(Token::kLParen)] = 150;
    t[static_cast<size_t>(Token::kPeriod)] = 150;
    return t;
  }();

  return table[static_cast<size_t>(token)];
}


static int BinaryPrecedence(Token token) {
  static int* table = [] {
    static int t[TokenCount()];
    for (size_t i = 0; i < TokenCount(); ++i) {
      t[i] = -1;
    }

    t[static_cast<size_t>(Token::kMul)] = 130;
    t[static_cast<size_t>(Token::kDiv)] = 130;
    t[static_cast<size_t>(Token::kRem)] = 130;
    t[static_cast<size_t>(Token::kAdd)] = 120;
    t[static_cast<size_t>(Token::kSub)] = 120;
    t[static_cast<size_t>(Token::kShl)] = 110;
    t[static_cast<size_t>(Token::kShr)] = 110;

    t[static_cast<size_t>(Token::kLT)] = 100;
    t[static_cast<size_t>(Token::kLE)] = 100;
    t[static_cast<size_t>(Token::kGT)] = 100;
    t[static_cast<size_t>(Token::kGE)] = 100;
    t[static_cast<size_t>(Token::kEQ)] = 90;
    t[static_cast<size_t>(Token::kNE)] = 90;

    t[static_cast<size_t>(Token::kAnd)] = 80;
    t[static_cast<size_t>(Token::kXor)] = 70;
    t[static_cast<size_t>(Token::kOr)] = 60;
    t[static_cast<size_t>(Token::kLAnd)] = 50;
    t[static_cast<size_t>(Token::kLOr)] = 40;

    t[static_cast<size_t>(Token::kAssign)] = 20;
    t[static_cast<size_t>(Token::kAddAssign)] = 20;
    t[static_cast<size_t>(Token::kSubAssign)] = 20;
    t[static_cast<size_t>(Token::kMulAssign)] = 20;
    t[static_cast<size_t>(Token::kDivAssign)] = 20;
    t[static_cast<size_t>(Token::kRemAssign)] = 20;
    t[static_cast<size_t>(Token::kAndAssign)] = 20;
    t[static_cast<size_t>(Token::kOrAssign)] = 20;
    t[static_cast<size_t>(Token::kXorAssign)] = 20;
    t[static_cast<size_t>(Token::kShlAssign)] = 20;
    t[static_cast<size_t>(Token::kShrAssign)] = 20;

    return t;
  }();

  return table[static_cast<size_t>(token)];
}


static int TernaryPrecedence(Token token) {
  if (token == Token::kQuestion) {
    return 30;
  } else {
    return -1;
  }
}


static constexpr auto kDeclarationEndTokens = {Token::kInterface, Token::kClass, Token::kDef, Token::kRBrace};

static constexpr auto kLambdaEndTokens = {Token::kSemicolon, Token::kNewline,   Token::kRParen, Token::kRBrack,
                                          Token::kRBrace,    Token::kInterface, Token::kClass,  Token::kDef};

static constexpr auto kStatementEndTokens = {Token::kSemicolon, Token::kNewline, Token::kInterface,
                                             Token::kClass,     Token::kDef,     Token::kLet,
                                             Token::kVar,       Token::kReturn,  Token::kRBrace};

static constexpr auto kExpressionEndTokens = {Token::kSemicolon, Token::kNewline, Token::kComma,
                                              Token::kRParen,    Token::kRBrack,  Token::kRBrace};


FileASTPtr Parser::ParseFile() {
  auto file_ast = FileAST::Create();
  file_ast->set_scope(context_->root_scope()->CreateChild());

  while (PeekAhead() != Token::kEOF) {
    switch (PeekAhead()) {
      case Token::kInterface:
        file_ast->add_declaration(ParseInterfaceDeclaration(file_ast->scope()));
        break;

      case Token::kClass:
        file_ast->add_declaration(ParseClassDeclaration(file_ast->scope()));
        break;

      case Token::kDef:
        file_ast->add_declaration(ParseFunctionDeclaration(file_ast->scope()));
        break;

      default:
        ErrorUnexpected();
        Synchronize(kDeclarationEndTokens);
    }
  }

  return file_ast;
}


DeclarationPtr Parser::ParseInterfaceDeclaration(Scope* scope) {
  Expect(Token::kInterface);

  if (!Expect(Token::kIdentifier)) {
    Synchronize(kDeclarationEndTokens);
    return nullptr;
  }

  auto interface_name = LastTokenValue().as_identifier;
  auto& ident_pos = LastTokenPosition();
  auto symbol = Symbol::Create(Symbol::Kind::kClass, scope, interface_name, ident_pos);
  auto interface = InterfaceDeclaration::Create(std::move(symbol));
  auto inner_scope = scope->CreateChild();

  if (PeekAhead() == Token::kColon) {
    interface->set_supers(ParseSuperClasses(scope));
  }

  if (!Expect(Token::kLBrace)) {
    Synchronize(kDeclarationEndTokens);
    return nullptr;
  }

  while (PeekAhead() != Token::kRBrace && PeekAhead() != Token::kEOF) {
    switch (PeekAhead()) {
      case Token::kDef:
        interface->add_method(ParseFunctionDeclaration(inner_scope.get(), false));
        break;

      default:
        ErrorUnexpected();
        Synchronize({Token::kDef, Token::kRBrace});
        break;
    }
  }

  interface->set_scope(std::move(inner_scope));
  Expect(Token::kRBrace);
  ExpectEndOfStatement();
  return interface;
}


DeclarationPtr Parser::ParseClassDeclaration(Scope* scope) {
  Expect(Token::kClass);

  if (!Expect(Token::kIdentifier)) {
    Synchronize(kDeclarationEndTokens);
    return nullptr;
  }

  auto class_name = LastTokenValue().as_identifier;
  auto& ident_pos = LastTokenPosition();
  auto symbol = Symbol::Create(Symbol::Kind::kClass, scope, class_name, ident_pos);
  auto clazz = ClassDeclaration::Create(std::move(symbol));
  auto inner_scope = scope->CreateChild();

  if (PeekAhead() == Token::kColon) {
    clazz->set_supers(ParseSuperClasses(scope));
  }

  if (!Expect(Token::kLBrace)) {
    Synchronize(kDeclarationEndTokens);
    return nullptr;
  }

  while (PeekAhead() != Token::kRBrace && PeekAhead() != Token::kEOF) {
    switch (PeekAhead()) {
      case Token::kIdentifier: {
        clazz->add_field(ParseClassField(inner_scope.get()));
        break;
      }

      case Token::kEmbed:
        clazz->add_embedding(ParseEmbeddingClass(inner_scope.get()));
        break;

      case Token::kInterface:
        ReportError(PeekTokenPosition(), "unsupported interface declaration inside class");
        clazz->add_declaration(ParseInterfaceDeclaration(inner_scope.get()));
        break;

      case Token::kClass:
        ReportError(PeekTokenPosition(), "unsupported class declaration inside class");
        clazz->add_declaration(ParseClassDeclaration(inner_scope.get()));
        break;

      case Token::kDef:
        clazz->add_declaration(ParseFunctionDeclaration(inner_scope.get()));
        break;

      default:
        ErrorUnexpected();
        Synchronize(kDeclarationEndTokens);
        break;
    }
  }

  clazz->set_scope(std::move(inner_scope));
  Expect(Token::kRBrace);
  ExpectEndOfStatement();
  return clazz;
}


ClassFieldPtr Parser::ParseClassField(Scope* scope) {
  Expect(Token::kIdentifier);
  auto field_name = LastTokenValue().as_identifier;
  auto& field_ident_pos = LastTokenPosition();
  auto field_symbol = Symbol::Create(Symbol::Kind::kVarVariable, scope, field_name, field_ident_pos);
  auto field = ClassField::Create(std::move(field_symbol));

  if (Expect(Token::kColon)) {
    field->set_type_repr(ParseTypeRepr());
  } else {
    Synchronize(kExpressionEndTokens);
  }

  ExpectEndOfStatement();
  return field;
}


EmbeddingClassPtr Parser::ParseEmbeddingClass(Scope* scope) {
  Expect(Token::kEmbed);
  Expect(Token::kIdentifier);
  auto embed_name = LastTokenValue().as_identifier;
  auto& embed_ident_pos = LastTokenPosition();
  auto embedding_class = EmbeddingClass::Create(embed_name);
  embedding_class->set_position(embed_ident_pos);

  ExpectEndOfStatement();
  return embedding_class;
}


Vector<SuperClassPtr> Parser::ParseSuperClasses(Scope* scope) {
  Expect(Token::kColon);
  Vector<SuperClassPtr> supers;

  bool first = true;
  while (PeekAhead() == Token::kIdentifier || PeekAhead() == Token::kComma) {
    if (!first) {
      Expect(Token::kComma);
    }
    first = false;

    if (Expect(Token::kIdentifier)) {
      auto super_name = LastTokenValue().as_identifier;
      auto& ident_pos = LastTokenPosition();
      auto super = SuperClass::Create(super_name);
      super->set_position(ident_pos);
      supers.push_back(std::move(super));
    } else {
      Synchronize(kDeclarationEndTokens);
      return supers;
    }
  }

  return supers;
}


FunctionDeclarationPtr Parser::ParseFunctionDeclaration(Scope* scope, bool needs_body) {
  Expect(Token::kDef);

  if (!Expect(Token::kIdentifier)) {
    Synchronize(kDeclarationEndTokens);
    return nullptr;
  }

  auto func_name = LastTokenValue().as_identifier;
  auto& ident_pos = LastTokenPosition();
  auto symbol = Symbol::Create(Symbol::Kind::kFunction, scope, func_name, ident_pos);

  auto func_expr = ParseLambdaExpression(scope, needs_body);
  if (func_expr == nullptr) {
    Synchronize(kDeclarationEndTokens);
    return nullptr;
  }

  ExpectEndOfStatement();
  return FunctionDeclaration::Create(std::move(symbol), std::move(func_expr));
}


FunctionExpressionPtr Parser::ParseLambdaExpression(Scope* scope, bool needs_body) {
  if (!Expect(Token::kLParen)) {
    Synchronize(kLambdaEndTokens);
    return nullptr;
  }

  auto func_position = LastTokenPosition();
  auto func_scope = scope->CreateChild();

  // parse parameters
  Vector<FunctionParamPtr> params;
  bool first = true;
  while (PeekAhead() == Token::kIdentifier || PeekAhead() == Token::kComma) {
    if (!first) {
      Expect(Token::kComma);
    }
    first = false;

    if (Expect(Token::kIdentifier)) {
      auto param_name = LastTokenValue().as_identifier;
      auto& ident_pos = LastTokenPosition();
      auto symbol = Symbol::Create(Symbol::Kind::kLetVariable, func_scope.get(), param_name, ident_pos);
      auto param = FunctionParam::Create(std::move(symbol));

      if (TryConsume(Token::kColon)) {
        param->set_type_repr(ParseTypeRepr());
      }
      params.push_back(std::move(param));

    } else {
      Synchronize(kLambdaEndTokens);
      return nullptr;
    }
  }
  Expect(Token::kRParen);

  FunctionExpressionPtr func_expr;
  TypeReprPtr return_type_repr = nullptr;

  if (TryConsume(Token::kColon)) {
    return_type_repr = ParseTypeRepr();
  }

  switch (PeekAhead()) {
    case Token::kLParen: {
      if (return_type_repr) {
        ErrorExpected(Token::kLBrace, Token::kDoubleArrow);
      }
      auto body_func = ParseLambdaExpression(func_scope.get(), needs_body);
      auto body_stmt = ReturnStatement::Create(std::move(body_func));
      auto body_block = Block::Create();
      body_block->add_statement(std::move(body_stmt));
      func_expr = FunctionExpression::Create(std::move(params), std::move(body_block));
      break;
    }

    case Token::kDoubleArrow: {
      Consume();
      auto body_expr = ParseExpression(func_scope.get());
      auto body_stmt = ReturnStatement::Create(std::move(body_expr));
      auto body_block = Block::Create();
      body_block->add_statement(std::move(body_stmt));
      func_expr = FunctionExpression::Create(std::move(params), std::move(body_block));
      break;
    }

    case Token::kLBrace: {
      auto body = ParseBlock(func_scope.get());
      func_expr = FunctionExpression::Create(std::move(params), std::move(body));
      break;
    }

    default:
      if (!needs_body) {
        func_expr = FunctionExpression::Create(std::move(params), nullptr);
        break;
      } else {
        ErrorUnexpected();
        Synchronize(kLambdaEndTokens);
        return nullptr;
      }
  }

  func_expr->set_scope(std::move(func_scope));
  func_expr->set_return_type_repr(std::move(return_type_repr));
  func_expr->set_position(func_position);

  if (!needs_body && func_expr->body() != nullptr) {
    ReportError(func_expr->position(), "function body is not allowed here");
  }

  return func_expr;
}


BlockPtr Parser::ParseBlock(Scope* scope) {
  Expect(Token::kLBrace);
  auto block = Block::Create();

  while (PeekAhead() != Token::kRBrace && PeekAhead() != Token::kEOF) {
    switch (PeekAhead()) {
      case Token::kInterface:
        ReportError(PeekTokenPosition(), "unsupported interface declaration inside block");
        block->add_declaration(ParseInterfaceDeclaration(scope));
        break;

      case Token::kClass:
        ReportError(PeekTokenPosition(), "unsupported class declaration inside block");
        block->add_declaration(ParseClassDeclaration(scope));
        break;

      case Token::kDef:
        block->add_declaration(ParseFunctionDeclaration(scope));
        break;

      case Token::kLet:
        block->add_statement(ParseLetStatement(scope));
        break;

      case Token::kVar:
        block->add_statement(ParseVarStatement(scope));
        break;

      case Token::kReturn:
        block->add_statement(ParseReturnStatement(scope));
        break;

      case Token::kSemicolon:
      case Token::kNewline:
        Consume();  // skip empty statement
        break;

      case Token::kLBrace:
      case Token::kLParen:
      case Token::kTrue:
      case Token::kFalse:
      case Token::kInteger:
      case Token::kFloat:
      case Token::kChar:
      case Token::kString:
      case Token::kThis:
      case Token::kIdentifier:
      case Token::kNew:
      case Token::kFn:
      case Token::kIf:
      case Token::kAdd:
      case Token::kSub:
      case Token::kNot:
      case Token::kLNot:
        block->add_statement(ParseExpressionStatement(scope));
        break;

      default:
        ErrorUnexpected();
        Synchronize(kStatementEndTokens);
        break;
    }
  }

  Expect(Token::kRBrace);
  return block;
}


StatementPtr Parser::ParseExpressionStatement(Scope* scope) {
  auto expr_pos = PeekTokenPosition();
  auto expr = ParseExpression(scope);

  if (!ExpectEndOfStatement()) {
    Synchronize(kStatementEndTokens);
  }

  auto stmt = ExpressionStatement::Create(std::move(expr));
  stmt->set_position(SourceRange(expr_pos, LastTokenPosition()));
  return stmt;
}


StatementPtr Parser::ParseLetStatement(Scope* scope) {
  Expect(Token::kLet);
  auto let_pos = LastTokenPosition();

  if (!Expect(Token::kIdentifier)) {
    Synchronize(kStatementEndTokens);
    return nullptr;
  }

  auto var_name = LastTokenValue().as_identifier;
  auto& ident_pos = LastTokenPosition();
  auto symbol = Symbol::Create(Symbol::Kind::kLetVariable, scope, var_name, ident_pos);

  Expect(Token::kAssign);
  auto expr = ParseExpression(scope);

  if (!ExpectEndOfStatement()) {
    Synchronize(kStatementEndTokens);
  }

  auto stmt = LetStatement::Create(std::move(symbol), std::move(expr));
  stmt->set_position(SourceRange(let_pos, LastTokenPosition()));
  return stmt;
}


StatementPtr Parser::ParseVarStatement(Scope* scope) {
  Expect(Token::kVar);
  auto var_pos = LastTokenPosition();

  if (!Expect(Token::kIdentifier)) {
    Synchronize(kStatementEndTokens);
    return nullptr;
  }

  auto var_name = LastTokenValue().as_identifier;
  auto& ident_pos = LastTokenPosition();
  auto symbol = Symbol::Create(Symbol::Kind::kVarVariable, scope, var_name, ident_pos);

  Expect(Token::kAssign);
  auto expr = ParseExpression(scope);

  if (!ExpectEndOfStatement()) {
    Synchronize(kStatementEndTokens);
  }

  auto stmt = VarStatement::Create(std::move(symbol), std::move(expr));
  stmt->set_position(SourceRange(var_pos, LastTokenPosition()));
  return stmt;
}


StatementPtr Parser::ParseReturnStatement(Scope* scope) {
  Expect(Token::kReturn);
  auto return_pos = LastTokenPosition();

  ExpressionPtr expr;
  if (TryConsumeEndOfStatement()) {
    expr = NullExpression::Create();
  } else {
    expr = ParseExpression(scope);
    ExpectEndOfStatement();
  }

  auto stmt = ReturnStatement::Create(std::move(expr));
  stmt->set_position(SourceRange(return_pos, LastTokenPosition()));
  return stmt;
}


TupleExpressionPtr Parser::ParseTupleExpression(Scope* scope) {
  auto tuple = TupleExpression::Create();
  bool first = true;
  while (PeekAhead() != Token::kRParen && PeekAhead() != Token::kRBrace && PeekAhead() != Token::kEOF) {
    if (!first) {
      Expect(Token::kComma);
    }
    first = false;

    tuple->add_element(ParseExpression(scope));
  }

  return tuple;
}


ExpressionPtr Parser::ParseExpression(Scope* scope, int min_precedence) {
  return ParseTernaryExpression(scope, min_precedence);
}


ExpressionPtr Parser::ParseTernaryExpression(Scope* scope, int min_precedence) {
  auto cond_expr = ParseBinaryExpression(scope, min_precedence);

  int prec = TernaryPrecedence(PeekAhead());
  if (prec < min_precedence) {
    return cond_expr;
  }

  Expect(Token::kQuestion);
  auto token_pos = LastTokenPosition();
  auto then_expr = ParseExpression(scope);
  Expect(Token::kColon);
  auto else_expr = ParseExpression(scope);

  auto expr = ConditionalExpression::Create(std::move(cond_expr), std::move(then_expr), std::move(else_expr));
  expr->set_position(token_pos);
  return expr;
}


ExpressionPtr Parser::ParseBinaryExpression(Scope* scope, int min_precedence) {
  auto lhs = ParseUnaryExpression(scope, min_precedence);

  while (true) {
    int prec = BinaryPrecedence(PeekAhead());
    if (prec < min_precedence) {
      break;
    }

    Token op = Consume();
    auto token_pos = LastTokenPosition();
    auto assoc = GetAssociativity(op);
    int next_min_prec = (assoc == Associativity::kLeft) ? prec + 1 : prec;

    auto rhs = ParseExpression(scope, next_min_prec);
    lhs = BinaryExpression::Create(op, std::move(lhs), std::move(rhs));
    lhs->set_position(token_pos);
  }

  return lhs;
}


ExpressionPtr Parser::ParseUnaryExpression(Scope* scope, int min_precedence) {
  auto lhs = ParsePrefixExpression(scope);

  // postfix
  while (true) {
    if (PostfixPrecedence(PeekAhead()) < min_precedence) {
      return lhs;
    }

    switch (PeekAhead()) {
      case Token::kLParen: {
        auto start_pos = PeekTokenPosition();
        Expect(Token::kLParen);
        lhs = ApplyExpression::Create(std::move(lhs), ParseTupleExpression(scope));
        Expect(Token::kRParen);
        auto end_pos = LastTokenPosition();
        lhs->set_position(SourceRange(start_pos, end_pos));
        break;
      }

      case Token::kPeriod: {
        Consume();
        if (Expect(Token::kIdentifier)) {
          auto field_name = LastTokenValue().as_identifier;
          auto& field_ident_pos = LastTokenPosition();
          lhs = ProjectionExpression::Create(std::move(lhs), field_name);
          lhs->set_position(field_ident_pos);
        } else {
          Synchronize(kExpressionEndTokens);
          return lhs;
        }
        break;
      }

      default:
        xylo_unreachable();
        return lhs;
    }
  }
}


ExpressionPtr Parser::ParsePrefixExpression(Scope* scope) {
  int prec = PrefixPrecedence(PeekAhead());
  if (prec > 0) {
    Token op = Consume();
    auto token_pos = LastTokenPosition();
    auto operand = ParseExpression(scope, prec);
    auto expr = UnaryExpression::Create(op, std::move(operand));
    expr->set_position(token_pos);
    return expr;
  } else {
    return ParsePrimaryExpression(scope);
  }
}


ExpressionPtr Parser::ParsePrimaryExpression(Scope* scope) {
  switch (PeekAhead()) {
    case Token::kLBrace: {
      auto start_pos = PeekTokenPosition();
      auto expr = BlockExpression::Create(ParseBlock(scope));
      expr->set_position(start_pos);
      return expr;
    }

    case Token::kLParen: {
      Expect(Token::kLParen);
      auto expr = ParseExpression(scope);
      Expect(Token::kRParen);
      return expr;
    }

    case Token::kTrue: {
      Consume();
      auto& token_pos = LastTokenPosition();
      auto expr = BooleanLiteral::Create(context_->bool_type(), true);
      expr->set_position(token_pos);
      return expr;
    }

    case Token::kFalse: {
      Consume();
      auto& token_pos = LastTokenPosition();
      auto expr = BooleanLiteral::Create(context_->bool_type(), false);
      expr->set_position(token_pos);
      return expr;
    }

    case Token::kInteger: {
      Consume();
      auto& token_pos = LastTokenPosition();
      auto expr = IntegerLiteral::Create(context_->int_type(), LastTokenValue().as_integer);
      expr->set_position(token_pos);
      return expr;
    }

    case Token::kFloat: {
      Consume();
      auto& token_pos = LastTokenPosition();
      auto expr = FloatLiteral::Create(context_->float_type(), LastTokenValue().as_float);
      expr->set_position(token_pos);
      return expr;
    }

    case Token::kThis: {
      Consume();
      auto& token_pos = LastTokenPosition();
      auto expr = ThisExpression::Create();
      expr->set_position(token_pos);
      return expr;
    }

    case Token::kIdentifier: {
      Consume();
      auto& token_pos = LastTokenPosition();
      auto expr = IdentifierExpression::Create(LastTokenValue().as_identifier);
      expr->set_position(token_pos);
      return expr;
    }

    case Token::kFn: {
      Consume();
      auto func_expr = ParseLambdaExpression(scope);
      if (func_expr == nullptr) {
        Synchronize(kExpressionEndTokens);
        return NullExpression::Create();
      } else {
        return func_expr;
      }
    }

    case Token::kIf:
      return ParseConditionalExpression(scope);

    case Token::kNew:
      return ParseConstructExpression(scope);

    default:
      ErrorUnexpected();
      Synchronize(kExpressionEndTokens);
      return NullExpression::Create();
  }
}


ExpressionPtr Parser::ParseConditionalExpression(Scope* scope) {
  Expect(Token::kIf);
  auto if_position = LastTokenPosition();

  Expect(Token::kLParen);
  auto condition = ParseExpression(scope);
  Expect(Token::kRParen);

  auto then_expr = ParseExpression(scope);

  ExpressionPtr else_expr;
  if (TryConsume(Token::kElse)) {
    else_expr = ParseExpression(scope);
  } else {
    else_expr = NullExpression::Create();
  }

  auto expr = ConditionalExpression::Create(std::move(condition), std::move(then_expr), std::move(else_expr));
  expr->set_position(SourceRange(if_position, LastTokenPosition()));
  return expr;
}


ExpressionPtr Parser::ParseConstructExpression(Scope* scope) {
  Expect(Token::kNew);
  Expect(Token::kIdentifier);
  auto class_name = LastTokenValue().as_identifier;
  auto ident_pos = LastTokenPosition();

  if (PeekAhead() != Token::kLBrace) {
    ErrorExpected(Token::kLBrace);
    Synchronize(kExpressionEndTokens);
    return NullExpression::Create();
  }

  auto initializer = ParseObjectInitializer(scope);
  auto expr = ConstructExpression::Create(class_name, std::move(initializer));
  expr->set_position(ident_pos);

  return expr;
}


ObjectInitializerPtr Parser::ParseObjectInitializer(Scope* scope) {
  Expect(Token::kLBrace);

  if (PeekAhead() != Token::kIdentifier && PeekAhead() != Token::kRBrace) {
    ErrorExpected(Token::kIdentifier, Token::kRBrace);
    Synchronize(kExpressionEndTokens);
    return nullptr;
  }

  auto initializer = ObjectInitializer::Create();

  while (PeekAhead() == Token::kIdentifier) {
    Expect(Token::kIdentifier);
    auto field_name = LastTokenValue().as_identifier;
    auto field_ident_pos = LastTokenPosition();

    if (Expect(Token::kColon)) {
      InitializerPtr value = nullptr;
      if (PeekAhead() == Token::kLBrace) {
        value = ParseObjectInitializer(scope);
      } else {
        value = ParseExpressionInitializer(scope);
      }

      auto entry = FieldEntry::Create(field_name, std::move(value));
      entry->set_position(field_ident_pos);
      initializer->add_entry(std::move(entry));
    } else {
      Synchronize(kExpressionEndTokens);
    }

    if (PeekAhead() != Token::kRBrace) {
      if (!Expect(Token::kComma)) {
        Synchronize(kExpressionEndTokens);
      }
    }
  }

  Expect(Token::kRBrace);
  return initializer;
}


InitializerPtr Parser::ParseExpressionInitializer(Scope* scope) {
  auto expr = ParseExpression(scope);
  auto initializer = ExpressionInitializer::Create(std::move(expr));
  return initializer;
}


TypeReprPtr Parser::ParseTypeRepr() {
  return ParseFunctionTypeRepr();
}


TypeReprPtr Parser::ParseFunctionTypeRepr() {
  auto lhs = ParsePrimaryTypeRepr();

  if (PeekAhead() == Token::kArrow) {
    Consume();
    auto token_pos = LastTokenPosition();

    TupleTypeReprPtr params_type;
    if (lhs->kind() == TypeRepr::Kind::kTuple) {
      params_type = TupleTypeReprPtr(lhs.release()->As<TupleTypeRepr>());
    } else {
      params_type = TupleTypeRepr::Create();
      params_type->add_element(std::move(lhs));
    }

    auto return_type = ParseFunctionTypeRepr();
    auto func_type_repr = FunctionTypeRepr::Create(std::move(params_type), std::move(return_type));
    func_type_repr->set_position(token_pos);
    return func_type_repr;
  } else {
    return lhs;
  }
}


TypeReprPtr Parser::ParsePrimaryTypeRepr() {
  switch (PeekAhead()) {
    case Token::kLParen: {
      Expect(Token::kLParen);
      auto tuple_type_repr = ParseTupleTypeRepr();
      Expect(Token::kRParen);
      return tuple_type_repr;
    }

    case Token::kIdentifier: {
      Consume();
      auto& ident_pos = LastTokenPosition();
      auto type_repr = NamedTypeRepr::Create(LastTokenValue().as_identifier);
      type_repr->set_position(ident_pos);
      return type_repr;
    }

    default:
      ErrorUnexpected();
      Synchronize(kExpressionEndTokens);
      return NullTypeRepr::Create();
  }
}


TypeReprPtr Parser::ParseTupleTypeRepr() {
  if (PeekAhead() == Token::kRParen) {
    auto tuple = TupleTypeRepr::Create();  // empty tuple
    tuple->set_position(LastTokenPosition());
    return tuple;
  }

  // take one element
  auto element = ParseTypeRepr();

  if (PeekAhead() == Token::kComma) {
    auto tuple = TupleTypeRepr::Create();
    tuple->add_element(std::move(element));
    tuple->set_position(PeekTokenPosition());

    while (PeekAhead() == Token::kComma) {
      Consume();
      tuple->add_element(ParseTypeRepr());
    }

    return tuple;
  } else {
    return element;
  }
}


void Parser::ErrorExpected(Token expected) {
  auto actual = PeekAhead();
  std::ostringstream oss;
  oss << "expected " << TokenPrinter(expected) << ", found " << TokenPrinter(actual, &PeekTokenValue());
  ReportError(PeekTokenPosition(), oss.str());
}


void Parser::ErrorExpected(Token expected1, Token expected2) {
  auto actual = PeekAhead();
  std::ostringstream oss;
  oss << "expected " << TokenPrinter(expected1) << " or " << TokenPrinter(expected2) << ", found "
      << TokenPrinter(actual, &PeekTokenValue());
  ReportError(PeekTokenPosition(), oss.str());
}


void Parser::ErrorUnexpected() {
  auto actual = PeekAhead();
  std::ostringstream oss;
  oss << "unexpected " << TokenPrinter(actual, &PeekTokenValue());
  ReportError(PeekTokenPosition(), oss.str());
}


void Parser::Synchronize(std::initializer_list<Token> sync) {
  auto in_sync = [&](Token t) {
    return std::find(sync.begin(), sync.end(), t) != sync.end();
  };

  int count_paren = 0;
  int count_brack = 0;
  int count_brace = 0;

  while (PeekAhead() != Token::kEOF) {
    if (count_paren == 0 && count_brack == 0 && count_brace == 0) {
      if (in_sync(PeekAhead())) {
        break;
      }
    }

    switch (PeekAhead()) {
      case Token::kLParen:
        count_paren++;
        break;
      case Token::kRParen:
        if (count_paren > 0) count_paren--;
        break;
      case Token::kLBrack:
        count_brack++;
        break;
      case Token::kRBrack:
        if (count_brack > 0) count_brack--;
        break;
      case Token::kLBrace:
        count_brace++;
        break;
      case Token::kRBrace:
        if (count_brace > 0) count_brace--;
        break;
      default:
        break;
    }

    Consume();
  }
}


}  // namespace xylo
