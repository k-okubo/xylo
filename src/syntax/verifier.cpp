
#include "xylo/syntax/verifier.h"

#include "xylo/syntax/substitution.h"

namespace xylo {


void Verifier::VisitFileAST(FileAST* file_ast) {
  for (auto& decl : file_ast->declarations()) {
    VisitDeclaration(decl.get());
  }
}


void Verifier::VisitDeclaration(Declaration* decl) {
  switch (decl->kind()) {
    case Declaration::Kind::kInterface:
      // Interfaces do not have bodies to visit.
      break;

    case Declaration::Kind::kClass:
      VisitClassDeclaration(decl->As<ClassDeclaration>());
      break;

    case Declaration::Kind::kFunction:
      VisitFunctionDeclaration(decl->As<FunctionDeclaration>());
      break;
  }
}


void Verifier::VisitClassDeclaration(ClassDeclaration* decl) {
  for (auto& inner_decl : decl->declarations()) {
    VisitDeclaration(inner_decl.get());
  }
}


void Verifier::VisitFunctionDeclaration(FunctionDeclaration* decl) {
  VisitFunctionExpression(decl->func());
}


void Verifier::VisitBlock(Block* block, FlowState* state) {
  for (auto& decl : block->declarations()) {
    VisitDeclaration(decl.get());
  }
  for (auto& stmt : block->statements()) {
    VisitStatement(stmt.get(), state);
  }
}


void Verifier::VisitStatement(Statement* stmt, FlowState* state) {
  if (!state->reachable && !state->unreachable_stmt_reported) {
    ReportError(stmt->position(), "unreachable statement");
    state->unreachable_stmt_reported = true;
  }

  switch (stmt->kind()) {
    case Statement::Kind::kExpression:
      VisitExpressionStatement(stmt->As<ExpressionStatement>(), state);
      break;

    case Statement::Kind::kLet:
      VisitLetStatement(stmt->As<LetStatement>(), state);
      break;

    case Statement::Kind::kVar:
      VisitVarStatement(stmt->As<VarStatement>(), state);
      break;

    case Statement::Kind::kReturn:
      VisitReturnStatement(stmt->As<ReturnStatement>(), state);
      break;
  }
}


void Verifier::VisitExpressionStatement(ExpressionStatement* stmt, FlowState* state) {
  VisitExpression(stmt->expr(), state);
}


void Verifier::VisitLetStatement(LetStatement* stmt, FlowState* state) {
  VisitExpression(stmt->expr(), state);
}


void Verifier::VisitVarStatement(VarStatement* stmt, FlowState* state) {
  VisitExpression(stmt->expr(), state);
}


void Verifier::VisitReturnStatement(ReturnStatement* stmt, FlowState* state) {
  VisitExpression(stmt->expr(), state);
  state->reachable = false;
}


void Verifier::VisitExpression(Expression* expr, FlowState* state) {
  switch (expr->kind()) {
    case Expression::Kind::kNull:
      break;

    case Expression::Kind::kLiteral:
      break;

    case Expression::Kind::kThis:
      break;

    case Expression::Kind::kIdentifier:
      break;

    case Expression::Kind::kTuple:
      VisitTupleExpression(expr->As<TupleExpression>(), state);
      break;

    case Expression::Kind::kFunction:
      VisitFunctionExpression(expr->As<FunctionExpression>());
      break;

    case Expression::Kind::kApply:
      VisitApplyExpression(expr->As<ApplyExpression>(), state);
      break;

    case Expression::Kind::kUnary:
      VisitUnaryExpression(expr->As<UnaryExpression>(), state);
      break;

    case Expression::Kind::kBinary:
      VisitBinaryExpression(expr->As<BinaryExpression>(), state);
      break;

    case Expression::Kind::kConditional:
      VisitConditionalExpression(expr->As<ConditionalExpression>(), state);
      break;

    case Expression::Kind::kNew:
      VisitNewExpression(expr->As<NewExpression>(), state);
      break;

    case Expression::Kind::kProjection:
      VisitProjectionExpression(expr->As<ProjectionExpression>(), state);
      break;

    case Expression::Kind::kBlock:
      VisitBlockExpression(expr->As<BlockExpression>(), state);
      break;
  }
}


void Verifier::VisitTupleExpression(TupleExpression* expr, FlowState* state) {
  for (auto& element : expr->elements()) {
    VisitExpression(element.get(), state);
  }
}


void Verifier::VisitFunctionExpression(FunctionExpression* expr) {
  FlowState local_state = {.reachable = true, .unreachable_stmt_reported = false};
  VisitBlock(expr->body(), &local_state);

  if (local_state.reachable) {
    auto return_type = expr->type()->As<FunctionType>()->return_type();
    if (return_type->IsSubtypeOf(context_->unit_type())) {
      auto ret_expr = NullExpression::Create();
      ret_expr->set_type(context_->unit_type());
      expr->body()->add_statement(ReturnStatement::Create(std::move(ret_expr)));
    } else {
      ReportError(expr->position(), "not all paths return a value");
    }
  }
}


void Verifier::VisitApplyExpression(ApplyExpression* expr, FlowState* state) {
  VisitExpression(expr->func(), state);
  VisitExpression(expr->args(), state);
}


void Verifier::VisitUnaryExpression(UnaryExpression* expr, FlowState* state) {
  VisitExpression(expr->operand(), state);
}


void Verifier::VisitBinaryExpression(BinaryExpression* expr, FlowState* state) {
  VisitExpression(expr->lhs(), state);
  VisitExpression(expr->rhs(), state);
}


void Verifier::VisitConditionalExpression(ConditionalExpression* expr, FlowState* state) {
  VisitExpression(expr->cond(), state);

  FlowState then_state(*state);
  FlowState else_state(*state);
  VisitExpression(expr->then_expr(), &then_state);
  VisitExpression(expr->else_expr(), &else_state);

  expr->set_reachable_then(then_state.reachable);
  expr->set_reachable_else(else_state.reachable);

  state->reachable = then_state.reachable || else_state.reachable;
}


void Verifier::VisitNewExpression(NewExpression* expr, FlowState* state) {
  VisitObjectInitializer(expr->initializer(), state);
}


void Verifier::VisitProjectionExpression(ProjectionExpression* expr, FlowState* state) {
  VisitExpression(expr->object(), state);
}


void Verifier::VisitBlockExpression(BlockExpression* expr, FlowState* state) {
  VisitBlock(expr->block(), state);
}


void Verifier::VisitExpressionInitializer(ExpressionInitializer* init, FlowState* state) {
  VisitExpression(init->expr(), state);
}


void Verifier::VisitObjectInitializer(ObjectInitializer* init, FlowState* state) {
  for (auto& entry : init->entries()) {
    VisitFieldEntry(entry.get(), state);
  }
}


void Verifier::VisitFieldEntry(FieldEntry* entry, FlowState* state) {
  switch (entry->value()->kind()) {
    case Initializer::Kind::kExpression:
      VisitExpressionInitializer(entry->value()->As<ExpressionInitializer>(), state);
      break;

    case Initializer::Kind::kObject:
      VisitObjectInitializer(entry->value()->As<ObjectInitializer>(), state);
      break;
  }
}


}  // namespace xylo
