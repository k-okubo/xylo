
#ifndef XYLO_SYNTAX_VERIFIER_H_
#define XYLO_SYNTAX_VERIFIER_H_

#include "xylo/syntax/ast.h"
#include "xylo/syntax/context.h"
#include "xylo/syntax/diagnostic.h"

namespace xylo {


class Verifier : public DiagnosticReporter {
 public:
  explicit Verifier(XyloContext* context) :
      DiagnosticReporter(),
      context_(context) {}

  ~Verifier() = default;

  Verifier(const Verifier&) = delete;
  Verifier& operator=(const Verifier&) = delete;

  void VisitFileAST(FileAST* file_ast);

 protected:
  struct FlowState {
    bool reachable;
    bool unreachable_stmt_reported;
  };

  void VisitDeclaration(Declaration* decl);
  void VisitClassDeclaration(ClassDeclaration* decl);
  void VisitFunctionDeclaration(FunctionDeclaration* decl);

  void VisitBlock(Block* block, FlowState* state);

  void VisitStatement(Statement* stmt, FlowState* state);
  void VisitExpressionStatement(ExpressionStatement* stmt, FlowState* state);
  void VisitLetStatement(LetStatement* stmt, FlowState* state);
  void VisitVarStatement(VarStatement* stmt, FlowState* state);
  void VisitReturnStatement(ReturnStatement* stmt, FlowState* state);

  void VisitExpression(Expression* expr, FlowState* state);
  void VisitTupleExpression(TupleExpression* expr, FlowState* state);
  void VisitFunctionExpression(FunctionExpression* expr);
  void VisitApplyExpression(ApplyExpression* expr, FlowState* state);
  void VisitUnaryExpression(UnaryExpression* expr, FlowState* state);
  void VisitBinaryExpression(BinaryExpression* expr, FlowState* state);
  void VisitConditionalExpression(ConditionalExpression* expr, FlowState* state);
  void VisitConstructExpression(ConstructExpression* expr, FlowState* state);
  void VisitProjectionExpression(ProjectionExpression* expr, FlowState* state);
  void VisitBlockExpression(BlockExpression* expr, FlowState* state);

  void VisitExpressionInitializer(ExpressionInitializer* init, FlowState* state);
  void VisitObjectInitializer(ObjectInitializer* init, FlowState* state);
  void VisitFieldEntry(FieldEntry* entry, FlowState* state);

 protected:
  XyloContext* context_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_VERIFIER_H_
