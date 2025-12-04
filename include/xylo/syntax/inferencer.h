
#ifndef XYLO_SYNTAX_INFERENCER_H_
#define XYLO_SYNTAX_INFERENCER_H_

#include <functional>
#include <utility>

#include "xylo/syntax/ast.h"
#include "xylo/syntax/context.h"
#include "xylo/syntax/diagnostic.h"
#include "xylo/util/contract.h"
#include "xylo/util/map.h"
#include "xylo/util/pair_hash.h"
#include "xylo/util/vector.h"

namespace xylo {


class Inferencer : public DiagnosticReporter {
 public:
  explicit Inferencer(XyloContext* context) :
      DiagnosticReporter(),
      context_(context),
      entity_states_() {}

  ~Inferencer() = default;

  Inferencer(const Inferencer&) = delete;
  Inferencer& operator=(const Inferencer&) = delete;

  void VisitFileAST(FileAST* file_ast);

 protected:
  struct InferenceContext {
    FunctionExpression* enclosing_func;
    bool needs_value;
  };

  struct EntityState {
    std::function<void()> resolve;
    bool in_progress;
    bool done;
    Vector<std::function<void()>> on_update_type;

    template <typename T>
    explicit EntityState(T&& resolver) :
        resolve(std::forward<T>(resolver)),
        in_progress(false),
        done(false),
        on_update_type() {}
  };

  void PreVisitInterfaceCreation(InterfaceDeclaration* decl);
  void PreVisitClassCreation(ClassDeclaration* decl);

  void PreVisitDeclaration(Declaration* decl);
  void PreVisitInterfaceDeclaration(InterfaceDeclaration* decl);
  void PreVisitClassDeclaration(ClassDeclaration* decl);
  void PreVisitFunctionDeclaration(FunctionDeclaration* decl);

  void RegisterSupertypes(NominalType* nominal_type, const Vector<SuperClassPtr>& super_classes);
  void RegisterEmbeddings(NominalType* nominal_type, const Vector<EmbeddingClassPtr>& embeddings);
  void RegisterFields(NominalType* nominal_type, const Vector<ClassFieldPtr>& fields);
  void RegisterMethod(NominalType* nominal_type, FunctionDeclaration* method);
  void RegisterMethodAbstract(MemberInfo* member_info, FunctionDeclaration* method);
  void RegisterMethodConcrete(MemberInfo* member_info, FunctionDeclaration* method);

  struct ClassDeclareInfo {
    Declaration* class_decl;
    Map<Identifier*, FunctionDeclaration*> method_decls;
  };
  void CheckOverrides(NominalType* nominal_type, const ClassDeclareInfo& declare_info, bool is_class);

  void VisitDeclaration(Declaration* decl);
  void VisitClassDeclaration(ClassDeclaration* decl);
  void VisitFunctionDeclaration(FunctionDeclaration* decl);

  void VisitBlock(Block* block, InferenceContext* ctx);

  void VisitStatement(Statement* stmt, InferenceContext* ctx);
  void VisitExpressionStatement(ExpressionStatement* stmt, InferenceContext* ctx);
  void VisitLetStatement(LetStatement* stmt, InferenceContext* ctx);
  void VisitVarStatement(VarStatement* stmt, InferenceContext* ctx);
  void VisitReturnStatement(ReturnStatement* stmt, InferenceContext* ctx);

  void VisitExpression(Expression* expr, InferenceContext* ctx);
  void VisitNullExpression(NullExpression* expr, InferenceContext* ctx);
  void VisitLiteralExpression(LiteralExpression* expr, InferenceContext* ctx);
  void VisitThisExpression(ThisExpression* expr, InferenceContext* ctx);
  void VisitIdentifierExpression(IdentifierExpression* expr, InferenceContext* ctx);
  void VisitTupleExpression(TupleExpression* expr, InferenceContext* ctx);
  void VisitFunctionExpression(FunctionExpression* expr, InferenceContext* ctx);
  void VisitFunctionPrototype(FunctionExpression* expr, bool is_lambda);
  void VisitFunctionBody(FunctionExpression* expr);
  void VisitApplyExpression(ApplyExpression* expr, InferenceContext* ctx);
  void VisitUnaryExpression(UnaryExpression* expr, InferenceContext* ctx);
  void VisitBinaryExpression(BinaryExpression* expr, InferenceContext* ctx);
  void VisitConditionalExpression(ConditionalExpression* expr, InferenceContext* ctx);
  void VisitNewExpression(NewExpression* expr, InferenceContext* ctx);
  void VisitProjectionExpression(ProjectionExpression* expr, InferenceContext* ctx);
  void VisitBlockExpression(BlockExpression* expr, InferenceContext* ctx);

  struct VariableContext {
    const SourceRange& position;
    Identifier* name;
    Type* type;
  };

  void VisitExpressionInitializer(ExpressionInitializer* init, VariableContext* vctx, InferenceContext* ctx);
  void VisitObjectInitializer(ObjectInitializer* init, VariableContext* vctx, InferenceContext* ctx);
  void VisitFieldEntry(FieldEntry* entry, MemberInfo* member_info, InferenceContext* ctx);

  Type* ConvertDeclarableTypeRepr(TypeRepr* type_repr, TypeSink* allocated);
  Type* ConvertTypeRepr(TypeRepr* type_repr, TypeSink* allocated);
  Type* ConvertNamedTypeRepr(NamedTypeRepr* type_repr, TypeSink* allocated);
  FunctionType* ConvertFunctionTypeRepr(FunctionTypeRepr* type_repr, TypeSink* allocated);
  TupleType* ConvertTupleTypeRepr(TupleTypeRepr* type_repr, TypeSink* allocated);

  EntityState& GetEntityState(Symbol* symbol);
  void RegisterEntityState(Symbol* symbol, EntityState&& state);
  bool MarkLValue(Expression* expr);

 private:
  XyloContext* context_;
  Map<Symbol*, EntityState> entity_states_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_INFERENCER_H_
