
#ifndef XYLO_SYNTAX_RESOLVER_H_
#define XYLO_SYNTAX_RESOLVER_H_

#include <limits>

#include "xylo/syntax/ast.h"
#include "xylo/syntax/context.h"
#include "xylo/syntax/diagnostic.h"
#include "xylo/syntax/location.h"
#include "xylo/util/map.h"

namespace xylo {


class NameTable {
 public:
  explicit NameTable(const NameTable* parent) :
      parent_(parent) {}

  ~NameTable() = default;

  NameTable(const NameTable&) = delete;
  NameTable& operator=(const NameTable&) = delete;

  const Map<Identifier*, Symbol*>& entries() const { return table_; }

  Symbol* Lookup(Identifier* name) const {
    auto it = table_.find(name);
    if (it != table_.end()) {
      return it->second;
    }

    if (parent_ != nullptr) {
      return parent_->Lookup(name);
    }

    return nullptr;
  }

  bool Insert(Identifier* name, Symbol* symbol) { return table_.emplace(name, symbol).second; }

  const NameTable* parent_;
  Map<Identifier*, Symbol*> table_;
};


class Resolver : public DiagnosticReporter {
 public:
  explicit Resolver(XyloContext* context) :
      DiagnosticReporter(),
      context_(context) {}

  ~Resolver() = default;

  Resolver(const Resolver&) = delete;
  Resolver& operator=(const Resolver&) = delete;

  void VisitFileAST(FileAST* file_ast, const NameTable* external_name_table = nullptr);

 protected:
  struct ResolutionContext {
    NameTable* name_table;
    ClassDeclaration* enclosing_class;
    FunctionExpression* enclosing_func;
  };

  struct FunctionState {
    int reference_depth = std::numeric_limits<int>::max();
    Vector<std::function<void(const FunctionState&)>> on_update_reference_depth;
  };

  void PrevisitDeclaration(Declaration* decl, ResolutionContext* ctx);
  void VisitDeclaration(Declaration* decl, ResolutionContext* ctx);
  void VisitClassDeclaration(ClassDeclaration* decl, ResolutionContext* ctx);
  void VisitClassField(ClassField* field, ResolutionContext* ctx);
  void VisitEmbeddingClass(EmbeddingClass* embedding, ResolutionContext* ctx);
  void VisitFunctionDeclaration(FunctionDeclaration* decl, ResolutionContext* ctx);

  void VisitBlock(Block* block, ResolutionContext* ctx);

  void VisitStatement(Statement* stmt, ResolutionContext* ctx);
  void VisitExpressionStatement(ExpressionStatement* stmt, ResolutionContext* ctx);
  void VisitLetStatement(LetStatement* stmt, ResolutionContext* ctx);
  void VisitVarStatement(VarStatement* stmt, ResolutionContext* ctx);
  void VisitReturnStatement(ReturnStatement* stmt, ResolutionContext* ctx);

  void VisitExpression(Expression* expr, ResolutionContext* ctx);
  void VisitThisExpression(ThisExpression* expr, ResolutionContext* ctx);
  void VisitIdentifierExpression(IdentifierExpression* expr, ResolutionContext* ctx);
  void VisitTupleExpression(TupleExpression* expr, ResolutionContext* ctx);
  void VisitFunctionExpression(FunctionExpression* expr, ResolutionContext* ctx, bool is_lambda);
  void VisitApplyExpression(ApplyExpression* expr, ResolutionContext* ctx);
  void VisitUnaryExpression(UnaryExpression* expr, ResolutionContext* ctx);
  void VisitBinaryExpression(BinaryExpression* expr, ResolutionContext* ctx);
  void VisitConditionalExpression(ConditionalExpression* expr, ResolutionContext* ctx);
  void VisitNewExpression(NewExpression* expr, ResolutionContext* ctx);
  void VisitProjectionExpression(ProjectionExpression* expr, ResolutionContext* ctx);
  void VisitBlockExpression(BlockExpression* expr, ResolutionContext* ctx);

  void VisitExpressionInitializer(ExpressionInitializer* initializer, ResolutionContext* ctx);
  void VisitObjectInitializer(ObjectInitializer* initializer, ResolutionContext* ctx);
  void VisitFieldEntry(FieldEntry* entry, ResolutionContext* ctx);

  void VisitTypeRepr(TypeRepr* type_repr, ResolutionContext* ctx);
  void VisitNamedTypeRepr(NamedTypeRepr* type_repr, ResolutionContext* ctx);
  void VisitFunctionTypeRepr(FunctionTypeRepr* type_repr, ResolutionContext* ctx);
  void VisitTupleTypeRepr(TupleTypeRepr* type_repr, ResolutionContext* ctx);

  void CollectCapturedSymbols(FunctionExpression* func);
  void CollectCapturedSymbols(FunctionExpression* func, Block* block);
  void MarkCapturedSymbol(FunctionExpression* func, Symbol* symbol);

  FunctionExpression* GetFunction(Symbol* symbol);
  FunctionState& GetFunctionState(FunctionExpression* func);

  void RegisterDecledFunction(Symbol* symbol, FunctionExpression* func);
  void RegisterLambdaFunction(FunctionExpression* func);
  void OnUpdateReferencedDepth(FunctionExpression* func, std::function<void(const FunctionState&)>&& callback);
  void InvokeUpdateReferencedDepth(const FunctionState& state);

  void MarkAsClosureIfNeeded(FunctionExpression* func, Symbol* reference_symbol);
  void MarkAsClosureIfNeeded(FunctionExpression* func, int reference_depth);

  void ErrorUndeclared(const SourceRange& position, Identifier* name);
  void ErrorRedefinition(const SourceRange& position, Identifier* name, const Symbol* previous_symbol);
  void ErrorTypeAsValue(const SourceRange& position, Symbol* symbol);
  void ErrorValueAsType(const SourceRange& position, Symbol* symbol);

 private:
  XyloContext* context_;
  Map<Symbol*, FunctionExpression*> function_map_;
  Map<FunctionExpression*, FunctionState> function_states_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_RESOLVER_H_
