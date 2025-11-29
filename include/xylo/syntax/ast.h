
#ifndef XYLO_SYNTAX_AST_H_
#define XYLO_SYNTAX_AST_H_

#include <memory>
#include <utility>

#include "xylo/syntax/identifier.h"
#include "xylo/syntax/location.h"
#include "xylo/syntax/token.h"
#include "xylo/syntax/type.h"
#include "xylo/util/downcastable.h"
#include "xylo/util/vector.h"

namespace xylo {

class Scope;
class Symbol;
class FileAST;
class Declaration;
class Block;
class Statement;
class Expression;
class TypeRepr;

class FunctionDeclaration;
class TupleExpression;
class FunctionExpression;
class TupleTypeRepr;

class ClassField;
class EmbeddingClass;
class FunctionParam;
class Initializer;
class ObjectInitializer;
class FieldEntry;

using ScopePtr = std::unique_ptr<Scope>;
using SymbolPtr = std::unique_ptr<Symbol>;
using FileASTPtr = std::unique_ptr<FileAST>;
using DeclarationPtr = std::unique_ptr<Declaration>;
using BlockPtr = std::unique_ptr<Block>;
using StatementPtr = std::unique_ptr<Statement>;
using ExpressionPtr = std::unique_ptr<Expression>;
using TypeReprPtr = std::unique_ptr<TypeRepr>;

using FunctionDeclarationPtr = std::unique_ptr<FunctionDeclaration>;
using TupleExpressionPtr = std::unique_ptr<TupleExpression>;
using FunctionExpressionPtr = std::unique_ptr<FunctionExpression>;
using TupleTypeReprPtr = std::unique_ptr<TupleTypeRepr>;

using ClassFieldPtr = std::unique_ptr<ClassField>;
using EmbeddingClassPtr = std::unique_ptr<EmbeddingClass>;
using FunctionParamPtr = std::unique_ptr<FunctionParam>;
using InitializerPtr = std::unique_ptr<Initializer>;
using ObjectInitializerPtr = std::unique_ptr<ObjectInitializer>;
using FieldEntryPtr = std::unique_ptr<FieldEntry>;


class Scope {
 public:
  static auto CreateRoot() {
    auto p = new Scope();
    return std::unique_ptr<Scope>(p);
  }

  static auto Create(const Scope* parent) {
    auto p = new Scope(parent);
    return std::unique_ptr<Scope>(p);
  }

  Scope() :
      parent_(nullptr),
      depth_(0) {}

  explicit Scope(const Scope* parent) :
      parent_(parent),
      depth_(parent->depth() + 1) {}

  ~Scope() = default;

  Scope(const Scope&) = delete;
  Scope& operator=(const Scope&) = delete;

  auto CreateChild() const { return Scope::Create(this); }

  const Scope* parent() const { return parent_; }

  int depth() const { return depth_; }

 private:
  const Scope* parent_;
  int depth_;
};


class Symbol {
 public:
  enum class Kind {
    kLetVariable,  // immutable
    kVarVariable,  // mutable
    kFunction,
    kClass,
  };

  static auto Create(Kind kind, Scope* scope, Identifier* name, const SourceRange& position) {
    auto p = new Symbol(kind, scope, name, position);
    return std::unique_ptr<Symbol>(p);
  }

  static auto Create(Kind kind, Scope* scope, Identifier* name) {
    auto p = new Symbol(kind, scope, name);
    return std::unique_ptr<Symbol>(p);
  }

  Symbol(Kind kind, Scope* scope, Identifier* name, const SourceRange& position) :
      kind_(kind),
      scope_(scope),
      name_(name),
      position_(position),
      type_(nullptr),
      captured_(false),
      captured_index_(0) {}

  Symbol(Kind kind, Scope* scope, Identifier* name) :
      Symbol(kind, scope, name, SourceRange()) {}

  ~Symbol() = default;

  Symbol(const Symbol&) = delete;
  Symbol& operator=(const Symbol&) = delete;

  Kind kind() const { return kind_; }
  Scope* scope() const { return scope_; }
  Identifier* name() const { return name_; }
  const SourceRange& position() const { return position_; }

  Type* type() const { return type_; }
  void set_type(Type* type) { type_ = type; }

  bool is_captured() const { return captured_; }
  void set_captured(bool captured) { captured_ = captured; }

  int captured_index() const { return captured_index_; }
  void set_captured_index(int index) { captured_index_ = index; }

 private:
  Kind kind_;
  Scope* scope_;
  Identifier* name_;
  SourceRange position_;
  Type* type_;
  bool captured_;
  int captured_index_;
};


class FileAST {
 public:
  static auto Create() {
    auto p = new FileAST();
    return std::unique_ptr<FileAST>(p);
  }

 protected:
  FileAST() :
      scope_(nullptr),
      declarations_(),
      entry_point_(nullptr) {}

 public:
  ~FileAST() = default;

  FileAST(const FileAST&) = delete;
  FileAST& operator=(const FileAST&) = delete;

  Scope* scope() const { return scope_.get(); }
  void set_scope(ScopePtr&& scope) { scope_ = std::move(scope); }

  const Vector<DeclarationPtr>& declarations() const { return declarations_; }
  void add_declaration(DeclarationPtr&& decl) { declarations_.push_back(std::move(decl)); }

  FunctionDeclaration* entry_point() const { return entry_point_; }
  void set_entry_point(FunctionDeclaration* decl) { entry_point_ = decl; }

 private:
  ScopePtr scope_;
  Vector<DeclarationPtr> declarations_;
  FunctionDeclaration* entry_point_;
};


class Declaration : public Downcastable {
 public:
  enum class Kind {
    kClass,
    kFunction,
  };

 protected:
  explicit Declaration(Kind kind, SymbolPtr&& symbol) :
      kind_(kind),
      symbol_(std::move(symbol)) {}

 public:
  virtual ~Declaration() = default;

  Declaration(const Declaration&) = delete;
  Declaration& operator=(const Declaration&) = delete;

  Kind kind() const { return kind_; }
  Symbol* symbol() const { return symbol_.get(); }

 private:
  Kind kind_;
  SymbolPtr symbol_;
};


class ClassDeclaration : public Declaration {
 public:
  static auto Create(SymbolPtr&& symbol) {
    return std::unique_ptr<ClassDeclaration>(new ClassDeclaration(std::move(symbol)));
  }

 protected:
  explicit ClassDeclaration(SymbolPtr&& symbol) :
      Declaration(Kind::kClass, std::move(symbol)),
      scope_(nullptr),
      fields_(),
      embeddings_(),
      declarations_() {}

 public:
  Scope* scope() const { return scope_.get(); }
  void set_scope(ScopePtr&& scope) { scope_ = std::move(scope); }

  const Vector<ClassFieldPtr>& fields() const { return fields_; }
  void add_field(ClassFieldPtr&& field) { fields_.push_back(std::move(field)); }

  const Vector<EmbeddingClassPtr>& embeddings() const { return embeddings_; }
  void add_embedding(EmbeddingClassPtr&& embedding) { embeddings_.push_back(std::move(embedding)); }

  const Vector<DeclarationPtr>& declarations() const { return declarations_; }
  void add_declaration(DeclarationPtr&& decl) { declarations_.push_back(std::move(decl)); }

 private:
  ScopePtr scope_;
  Vector<ClassFieldPtr> fields_;
  Vector<EmbeddingClassPtr> embeddings_;
  Vector<DeclarationPtr> declarations_;
};


class ClassField {
 public:
  static auto Create(SymbolPtr&& symbol) {
    auto p = new ClassField(std::move(symbol));
    return std::unique_ptr<ClassField>(p);
  }

 protected:
  explicit ClassField(SymbolPtr&& symbol) :
      symbol_(std::move(symbol)),
      type_repr_(nullptr) {}

 public:
  Symbol* symbol() const { return symbol_.get(); }

  TypeRepr* type_repr() const { return type_repr_.get(); }
  void set_type_repr(TypeReprPtr&& type_repr) { type_repr_ = std::move(type_repr); }

 private:
  SymbolPtr symbol_;
  TypeReprPtr type_repr_;
};


class EmbeddingClass {
 public:
  static auto Create(Identifier* name) {
    auto p = new EmbeddingClass(name);
    return std::unique_ptr<EmbeddingClass>(p);
  }

 protected:
  explicit EmbeddingClass(Identifier* name) :
      name_(name),
      position_(),
      symbol_(nullptr) {}

 public:
  Identifier* name() const { return name_; }

  const SourceRange& position() const { return position_; }
  void set_position(const SourceRange& position) { position_ = position; }

  Symbol* symbol() const { return symbol_; }
  void set_symbol(Symbol* symbol) { symbol_ = symbol; }

 private:
  Identifier* name_;
  SourceRange position_;
  Symbol* symbol_;
};


class FunctionDeclaration : public Declaration {
 public:
  static auto Create(SymbolPtr&& symbol, FunctionExpressionPtr&& func) {
    auto p = new FunctionDeclaration(std::move(symbol), std::move(func));
    return std::unique_ptr<FunctionDeclaration>(p);
  }

 protected:
  explicit FunctionDeclaration(SymbolPtr&& symbol, FunctionExpressionPtr&& func) :
      Declaration(Kind::kFunction, std::move(symbol)),
      func_(std::move(func)) {}

 public:
  FunctionExpression* func() const { return func_.get(); }

 private:
  FunctionExpressionPtr func_;
};


class Block {
 public:
  static auto Create() {
    auto p = new Block();
    return std::unique_ptr<Block>(p);
  }

 protected:
  Block() :
      statements_(),
      declarations_() {}

 public:
  ~Block() = default;

  Block(const Block&) = delete;
  Block& operator=(const Block&) = delete;

  const Vector<StatementPtr>& statements() const { return statements_; }
  void add_statement(StatementPtr&& stmt) { statements_.push_back(std::move(stmt)); }

  const Vector<DeclarationPtr>& declarations() const { return declarations_; }
  void add_declaration(DeclarationPtr&& decl) { declarations_.push_back(std::move(decl)); }

 private:
  Vector<StatementPtr> statements_;
  Vector<DeclarationPtr> declarations_;
};


class Statement : public Downcastable {
 public:
  enum class Kind {
    kExpression,
    kLet,
    kVar,
    kReturn,
  };

 protected:
  explicit Statement(Kind kind) :
      kind_(kind),
      position_() {}

 public:
  virtual ~Statement() = default;

  Statement(const Statement&) = delete;
  Statement& operator=(const Statement&) = delete;

  Kind kind() const { return kind_; }

  const SourceRange& position() const { return position_; }
  void set_position(const SourceRange& position) { position_ = position; }

 private:
  Kind kind_;
  SourceRange position_;
};


class ExpressionStatement : public Statement {
 public:
  static auto Create(ExpressionPtr&& expr) {
    auto p = new ExpressionStatement(std::move(expr));
    return std::unique_ptr<ExpressionStatement>(p);
  }

 protected:
  explicit ExpressionStatement(ExpressionPtr&& expr) :
      Statement(Kind::kExpression),
      expr_(std::move(expr)) {}

 public:
  Expression* expr() const { return expr_.get(); }

 private:
  ExpressionPtr expr_;
};


class LetStatement : public Statement {
 public:
  static auto Create(SymbolPtr&& symbol, ExpressionPtr&& expr) {
    return std::unique_ptr<LetStatement>(new LetStatement(std::move(symbol), std::move(expr)));
  }

 protected:
  explicit LetStatement(SymbolPtr&& symbol, ExpressionPtr&& expr) :
      Statement(Kind::kLet),
      symbol_(std::move(symbol)),
      expr_(std::move(expr)) {}

 public:
  Symbol* symbol() const { return symbol_.get(); }
  Expression* expr() const { return expr_.get(); }

 private:
  SymbolPtr symbol_;
  ExpressionPtr expr_;
};


class VarStatement : public Statement, public TypeSink {
 public:
  static auto Create(SymbolPtr&& symbol, ExpressionPtr&& expr) {
    auto p = new VarStatement(std::move(symbol), std::move(expr));
    return std::unique_ptr<VarStatement>(p);
  }

 protected:
  explicit VarStatement(SymbolPtr&& symbol, ExpressionPtr&& expr) :
      Statement(Kind::kVar),
      symbol_(std::move(symbol)),
      expr_(std::move(expr)) {}

 public:
  Symbol* symbol() const { return symbol_.get(); }
  Expression* expr() const { return expr_.get(); }

 private:
  SymbolPtr symbol_;
  ExpressionPtr expr_;
};


class ReturnStatement : public Statement {
 public:
  static auto Create(ExpressionPtr&& expr) {
    auto p = new ReturnStatement(std::move(expr));
    return std::unique_ptr<ReturnStatement>(p);
  }

 protected:
  explicit ReturnStatement(ExpressionPtr&& expr) :
      Statement(Kind::kReturn),
      expr_(std::move(expr)) {}

 public:
  Expression* expr() const { return expr_.get(); }

 private:
  ExpressionPtr expr_;
};


class Expression : public TypeSink, public Downcastable {
 public:
  enum class Kind {
    kNull,
    kLiteral,
    kThis,
    kIdentifier,
    kTuple,
    kFunction,
    kApply,
    kUnary,
    kBinary,
    kConditional,
    kNew,
    kProjection,
    kBlock,
  };

 protected:
  explicit Expression(Kind kind) :
      kind_(kind),
      type_(nullptr),
      position_() {}

 public:
  virtual ~Expression() = default;

  Expression(const Expression&) = delete;
  Expression& operator=(const Expression&) = delete;

  Kind kind() const { return kind_; }

  Type* type() const { return type_; }
  void set_type(Type* type) { type_ = type; }

  const SourceRange& position() const { return position_; }
  void set_position(const SourceRange& position) { position_ = position; }

 private:
  Kind kind_;
  Type* type_;
  SourceRange position_;
};


class NullExpression : public Expression {
 public:
  static auto Create() {
    auto p = new NullExpression();
    return std::unique_ptr<NullExpression>(p);
  }

 protected:
  NullExpression() :
      Expression(Kind::kNull) {}
};


class LiteralExpression : public Expression {
 public:
  enum class LiteralKind {
    kBoolean,
    kInteger,
    kFloat,
  };

 protected:
  LiteralExpression(LiteralKind kind, Type* type) :
      Expression(Kind::kLiteral),
      kind_(kind) {
    set_type(type);
  }

 public:
  LiteralKind literal_kind() const { return kind_; }

 private:
  LiteralKind kind_;
};


class BooleanLiteral : public LiteralExpression {
 public:
  static auto Create(Type* type, bool value) {
    auto p = new BooleanLiteral(type, value);
    return std::unique_ptr<BooleanLiteral>(p);
  }

 protected:
  BooleanLiteral(Type* type, bool value) :
      LiteralExpression(LiteralKind::kBoolean, type),
      value_(value) {}

 public:
  bool value() const { return value_; }

 private:
  bool value_;
};


class IntegerLiteral : public LiteralExpression {
 public:
  static auto Create(Type* type, int64_t value) {
    auto p = new IntegerLiteral(type, value);
    return std::unique_ptr<IntegerLiteral>(p);
  }

 protected:
  IntegerLiteral(Type* type, int64_t value) :
      LiteralExpression(LiteralKind::kInteger, type),
      value_(value) {}

 public:
  int64_t value() const { return value_; }

 private:
  int64_t value_;
};


class FloatLiteral : public LiteralExpression {
 public:
  static auto Create(Type* type, double value) {
    auto p = new FloatLiteral(type, value);
    return std::unique_ptr<FloatLiteral>(p);
  }

 protected:
  FloatLiteral(Type* type, double value) :
      LiteralExpression(LiteralKind::kFloat, type),
      value_(value) {}

 public:
  double value() const { return value_; }

 private:
  double value_;
};


class ThisExpression : public Expression {
 public:
  static auto Create() {
    auto p = new ThisExpression();
    return std::unique_ptr<ThisExpression>(p);
  }

 protected:
  ThisExpression() :
      Expression(Kind::kThis) {}

 public:
  Symbol* class_symbol() const { return class_symbol_; }
  void set_class_symbol(Symbol* class_symbol) { class_symbol_ = class_symbol; }

 private:
  Symbol* class_symbol_;
};


class IdentifierExpression : public Expression {
 public:
  static auto Create(Identifier* name) {
    auto p = new IdentifierExpression(name);
    return std::unique_ptr<IdentifierExpression>(p);
  }

 protected:
  explicit IdentifierExpression(Identifier* name) :
      Expression(Kind::kIdentifier),
      name_(name),
      symbol_(nullptr),
      instantiated_vars_(),
      lvalue_(false) {}

 public:
  Identifier* name() const { return name_; }

  Symbol* symbol() const { return symbol_; }
  void set_symbol(Symbol* symbol) { symbol_ = symbol; }

  const Vector<TypeMetavar*>& instantiated_vars() const { return instantiated_vars_; }
  void set_instantiated_vars(Vector<TypeMetavar*>&& vars) { instantiated_vars_ = std::move(vars); }

  bool is_lvalue() const { return lvalue_; }
  void set_lvalue(bool lvalue) { lvalue_ = lvalue; }

 private:
  Identifier* name_;
  Symbol* symbol_;
  Vector<TypeMetavar*> instantiated_vars_;
  bool lvalue_;
};


class TupleExpression : public Expression {
 public:
  static auto Create() {
    auto p = new TupleExpression();
    return std::unique_ptr<TupleExpression>(p);
  }

 protected:
  TupleExpression() :
      Expression(Kind::kTuple) {}

 public:
  const Vector<ExpressionPtr>& elements() const { return elements_; }
  void add_element(ExpressionPtr&& element) { elements_.push_back(std::move(element)); }

 private:
  Vector<ExpressionPtr> elements_;
};


class FunctionExpression : public Expression {
 public:
  static auto Create(Vector<FunctionParamPtr>&& params, BlockPtr&& body) {
    auto p = new FunctionExpression(std::move(params), std::move(body));
    return std::unique_ptr<FunctionExpression>(p);
  }

 protected:
  explicit FunctionExpression(Vector<FunctionParamPtr>&& params, BlockPtr&& body) :
      Expression(Kind::kFunction),
      params_(std::move(params)),
      body_(std::move(body)),
      scope_(nullptr),
      return_type_repr_(nullptr),
      closure_(false),
      has_closure_(false),
      captured_symbols_() {}

 public:
  const Vector<FunctionParamPtr>& params() const { return params_; }
  Block* body() const { return body_.get(); }

  Scope* scope() const { return scope_.get(); }
  void set_scope(ScopePtr&& scope) { scope_ = std::move(scope); }

  TypeRepr* return_type_repr() const { return return_type_repr_.get(); }
  void set_return_type_repr(TypeReprPtr&& type_repr) { return_type_repr_ = std::move(type_repr); }

  bool is_closure() const { return closure_; }
  void set_closure(bool closure) { closure_ = closure; }

  bool has_closure() const { return has_closure_; }
  void set_has_closure(bool has_closure) { has_closure_ = has_closure; }

  const Vector<Symbol*>& captured_symbols() const { return captured_symbols_; }
  void add_captured_symbol(Symbol* symbol) { captured_symbols_.push_back(symbol); }

 private:
  Vector<FunctionParamPtr> params_;
  BlockPtr body_;
  ScopePtr scope_;
  TypeReprPtr return_type_repr_;
  bool closure_;
  bool has_closure_;
  Vector<Symbol*> captured_symbols_;
};


class FunctionParam {
 public:
  static auto Create(SymbolPtr&& symbol) {
    auto p = new FunctionParam(std::move(symbol));
    return std::unique_ptr<FunctionParam>(p);
  }

 protected:
  explicit FunctionParam(SymbolPtr&& symbol) :
      symbol_(std::move(symbol)),
      type_repr_(nullptr) {}

 public:
  Symbol* symbol() const { return symbol_.get(); }

  TypeRepr* type_repr() const { return type_repr_.get(); }
  void set_type_repr(TypeReprPtr&& type_repr) { type_repr_ = std::move(type_repr); }

 private:
  SymbolPtr symbol_;
  TypeReprPtr type_repr_;
};


class ApplyExpression : public Expression {
 public:
  static auto Create(ExpressionPtr&& func, TupleExpressionPtr&& args) {
    auto p = new ApplyExpression(std::move(func), std::move(args));
    return std::unique_ptr<ApplyExpression>(p);
  }

 protected:
  explicit ApplyExpression(ExpressionPtr&& func, TupleExpressionPtr&& args) :
      Expression(Kind::kApply),
      func_(std::move(func)),
      args_(std::move(args)) {}

 public:
  Expression* func() const { return func_.get(); }
  TupleExpression* args() const { return args_.get(); }

 private:
  ExpressionPtr func_;
  TupleExpressionPtr args_;
};


class UnaryExpression : public Expression {
 public:
  static auto Create(Token op, ExpressionPtr&& operand) {
    auto p = new UnaryExpression(op, std::move(operand));
    return std::unique_ptr<UnaryExpression>(p);
  }

 protected:
  explicit UnaryExpression(Token op, ExpressionPtr&& operand) :
      Expression(Kind::kUnary),
      op_(op),
      operand_(std::move(operand)) {}

 public:
  Token op() const { return op_; }
  Expression* operand() const { return operand_.get(); }

 private:
  Token op_;
  ExpressionPtr operand_;
};


class BinaryExpression : public Expression {
 public:
  static auto Create(Token op, ExpressionPtr&& lhs, ExpressionPtr&& rhs) {
    auto p = new BinaryExpression(op, std::move(lhs), std::move(rhs));
    return std::unique_ptr<BinaryExpression>(p);
  }

 protected:
  explicit BinaryExpression(Token op, ExpressionPtr&& lhs, ExpressionPtr&& rhs) :
      Expression(Kind::kBinary),
      op_(op),
      lhs_(std::move(lhs)),
      rhs_(std::move(rhs)) {}

 public:
  Token op() const { return op_; }
  Expression* lhs() const { return lhs_.get(); }
  Expression* rhs() const { return rhs_.get(); }

 private:
  Token op_;
  ExpressionPtr lhs_;
  ExpressionPtr rhs_;
};


class ConditionalExpression : public Expression {
 public:
  static auto Create(ExpressionPtr&& cond, ExpressionPtr&& then_expr, ExpressionPtr&& else_expr) {
    auto p = new ConditionalExpression(std::move(cond), std::move(then_expr), std::move(else_expr));
    return std::unique_ptr<ConditionalExpression>(p);
  }

 protected:
  explicit ConditionalExpression(ExpressionPtr&& cond, ExpressionPtr&& then_expr, ExpressionPtr&& else_expr) :
      Expression(Kind::kConditional),
      cond_(std::move(cond)),
      then_expr_(std::move(then_expr)),
      else_expr_(std::move(else_expr)) {}

 public:
  Expression* cond() const { return cond_.get(); }
  Expression* then_expr() const { return then_expr_.get(); }
  Expression* else_expr() const { return else_expr_.get(); }

  bool is_reachable_then() const { return reachable_then_; }
  void set_reachable_then(bool reachable) { reachable_then_ = reachable; }

  bool is_reachable_else() const { return reachable_else_; }
  void set_reachable_else(bool reachable) { reachable_else_ = reachable; }

 private:
  ExpressionPtr cond_;
  ExpressionPtr then_expr_;
  ExpressionPtr else_expr_;
  bool reachable_then_;
  bool reachable_else_;
};


class NewExpression : public Expression {
 public:
  static auto Create(Identifier* class_name, ObjectInitializerPtr&& initializer) {
    auto p = new NewExpression(class_name, std::move(initializer));
    return std::unique_ptr<NewExpression>(p);
  }

 protected:
  explicit NewExpression(Identifier* class_name, ObjectInitializerPtr&& initializer) :
      Expression(Kind::kNew),
      class_name_(class_name),
      initializer_(std::move(initializer)),
      class_symbol_(nullptr) {}

 public:
  Identifier* class_name() const { return class_name_; }
  ObjectInitializer* initializer() const { return initializer_.get(); }

  Symbol* class_symbol() const { return class_symbol_; }
  void set_class_symbol(Symbol* class_symbol) { class_symbol_ = class_symbol; }

 private:
  Identifier* class_name_;
  ObjectInitializerPtr initializer_;
  Symbol* class_symbol_;
};


class ProjectionExpression : public Expression {
 public:
  static auto Create(ExpressionPtr&& object, Identifier* member_name) {
    auto p = new ProjectionExpression(std::move(object), member_name);
    return std::unique_ptr<ProjectionExpression>(p);
  }

 protected:
  explicit ProjectionExpression(ExpressionPtr&& object, Identifier* member_name) :
      Expression(Kind::kProjection),
      object_(std::move(object)),
      member_name_(member_name),
      member_req_(nullptr),
      lvalue_(false) {}

 public:
  Expression* object() const { return object_.get(); }
  Identifier* member_name() const { return member_name_; }

  MemberRequirement* member_req() const { return member_req_; }
  void set_member_req(MemberRequirement* member_req) { member_req_ = member_req; }

  bool is_lvalue() const { return lvalue_; }
  void set_lvalue(bool lvalue) { lvalue_ = lvalue; }

 private:
  ExpressionPtr object_;
  Identifier* member_name_;
  MemberRequirement* member_req_;
  bool lvalue_;
};


class BlockExpression : public Expression {
 public:
  static auto Create(BlockPtr&& block) {
    auto p = new BlockExpression(std::move(block));
    return std::unique_ptr<BlockExpression>(p);
  }

 protected:
  explicit BlockExpression(BlockPtr&& block) :
      Expression(Kind::kBlock),
      block_(std::move(block)) {}

 public:
  Block* block() const { return block_.get(); }

 private:
  BlockPtr block_;
};


class Initializer : public Downcastable {
 public:
  enum class Kind {
    kExpression,
    kObject,
  };

 protected:
  explicit Initializer(Kind kind) :
      kind_(kind) {}

 public:
  virtual ~Initializer() = default;

  Initializer(const Initializer&) = delete;
  Initializer& operator=(const Initializer&) = delete;

  Kind kind() const { return kind_; }

 private:
  Kind kind_;
};


class ExpressionInitializer : public Initializer {
 public:
  static auto Create(ExpressionPtr&& expr) {
    auto p = new ExpressionInitializer(std::move(expr));
    return std::unique_ptr<ExpressionInitializer>(p);
  }

 protected:
  explicit ExpressionInitializer(ExpressionPtr&& expr) :
      Initializer(Kind::kExpression),
      expr_(std::move(expr)) {}

 public:
  Expression* expr() const { return expr_.get(); }

 private:
  ExpressionPtr expr_;
};


class ObjectInitializer : public Initializer {
 public:
  static auto Create() {
    auto p = new ObjectInitializer();
    return std::unique_ptr<ObjectInitializer>(p);
  }

 protected:
  ObjectInitializer() :
      Initializer(Kind::kObject),
      entries_() {}

 public:
  const Vector<FieldEntryPtr>& entries() const { return entries_; }

  void add_entry(FieldEntryPtr&& entry) { entries_.push_back(std::move(entry)); }

 private:
  Vector<FieldEntryPtr> entries_;
};


class FieldEntry {
 public:
  static auto Create(Identifier* name, InitializerPtr&& value) {
    auto p = new FieldEntry(name, std::move(value));
    return std::unique_ptr<FieldEntry>(p);
  }

 protected:
  explicit FieldEntry(Identifier* name, InitializerPtr&& value) :
      name_(name),
      value_(std::move(value)),
      position_() {}

 public:
  Identifier* name() const { return name_; }
  Initializer* value() const { return value_.get(); }

  const SourceRange& position() const { return position_; }
  void set_position(const SourceRange& position) { position_ = position; }

 private:
  Identifier* name_;
  InitializerPtr value_;
  SourceRange position_;
};


class TypeRepr : public Downcastable {
 public:
  enum class Kind {
    kNull,
    kNamed,
    kFunction,
    kTuple,
  };

 protected:
  explicit TypeRepr(Kind kind) :
      kind_(kind) {}

 public:
  virtual ~TypeRepr() = default;

  TypeRepr(const TypeRepr&) = delete;
  TypeRepr& operator=(const TypeRepr&) = delete;

  Kind kind() const { return kind_; }

  const SourceRange& position() const { return position_; }
  void set_position(const SourceRange& position) { position_ = position; }

 private:
  Kind kind_;
  SourceRange position_;
};


class NullTypeRepr : public TypeRepr {
 public:
  static auto Create() {
    auto p = new NullTypeRepr();
    return std::unique_ptr<NullTypeRepr>(p);
  }

 protected:
  NullTypeRepr() :
      TypeRepr(Kind::kNull) {}
};


class NamedTypeRepr : public TypeRepr {
 public:
  static auto Create(Identifier* name) {
    auto p = new NamedTypeRepr(name);
    return std::unique_ptr<NamedTypeRepr>(p);
  }

 protected:
  explicit NamedTypeRepr(Identifier* name) :
      TypeRepr(Kind::kNamed),
      name_(name),
      symbol_(nullptr) {}

 public:
  Identifier* name() const { return name_; }

  Symbol* symbol() const { return symbol_; }
  void set_symbol(Symbol* symbol) { symbol_ = symbol; }

 private:
  Identifier* name_;
  Symbol* symbol_;
};


class FunctionTypeRepr : public TypeRepr {
 public:
  static auto Create(TupleTypeReprPtr&& params_type, TypeReprPtr&& return_type) {
    auto p = new FunctionTypeRepr(std::move(params_type), std::move(return_type));
    return std::unique_ptr<FunctionTypeRepr>(p);
  }

 protected:
  FunctionTypeRepr(TupleTypeReprPtr&& params_type, TypeReprPtr&& return_type) :
      TypeRepr(Kind::kFunction),
      params_type_(std::move(params_type)),
      return_type_(std::move(return_type)) {}

 public:
  TupleTypeRepr* params_type() const { return params_type_.get(); }
  TypeRepr* return_type() const { return return_type_.get(); }

 private:
  TupleTypeReprPtr params_type_;
  TypeReprPtr return_type_;
};


class TupleTypeRepr : public TypeRepr {
 public:
  static auto Create() {
    auto p = new TupleTypeRepr();
    return std::unique_ptr<TupleTypeRepr>(p);
  }

 protected:
  TupleTypeRepr() :
      TypeRepr(Kind::kTuple),
      elements_() {}

 public:
  const Vector<TypeReprPtr>& elements() const { return elements_; }
  void add_element(TypeReprPtr&& element) { elements_.push_back(std::move(element)); }

 private:
  Vector<TypeReprPtr> elements_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_AST_H_
