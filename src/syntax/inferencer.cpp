
#include "xylo/syntax/inferencer.h"

#include <sstream>

#include "xylo/syntax/substitution.h"

namespace xylo {


void Inferencer::VisitFileAST(FileAST* file_ast) {
  for (auto& decl : file_ast->declarations()) {
    if (decl->kind() == Declaration::Kind::kClass) {
      PreVisitClassRegistration(decl->As<ClassDeclaration>());
    }
  }

  for (auto& decl : file_ast->declarations()) {
    PrevisitDeclaration(decl.get());
  }

  for (auto& decl : file_ast->declarations()) {
    VisitDeclaration(decl.get());
  }
}


void Inferencer::PrevisitDeclaration(Declaration* decl) {
  switch (decl->kind()) {
    case Declaration::Kind::kClass:
      PrevisitClassDeclaration(decl->As<ClassDeclaration>());
      break;

    case Declaration::Kind::kFunction:
      PrevisitFunctionDeclaration(decl->As<FunctionDeclaration>());
      break;
  }
}


void Inferencer::PreVisitClassRegistration(ClassDeclaration* decl) {
  xylo_contract(decl->symbol()->type() == nullptr);

  auto class_type = new NominalType(decl->symbol()->name());
  decl->symbol()->set_type(class_type);
  context_->RegisterClass(decl->symbol(), TypePtr(class_type));
}


void Inferencer::PrevisitClassDeclaration(ClassDeclaration* decl) {
  xylo_contract(decl->symbol()->type() != nullptr);
  auto class_type = decl->symbol()->type()->As<NominalType>();

  // set fields
  for (auto& field : decl->fields()) {
    auto field_symbol = field->symbol();
    xylo_contract(field_symbol->type() == nullptr);

    auto field_type = ConvertDeclarableTypeRepr(field->type_repr(), class_type);
    field_symbol->set_type(field_type);

    auto member_info = class_type->AddField(field_symbol->name(), field_type);
    xylo_contract(member_info != nullptr);
    field_symbol->set_captured_index(member_info->index());
  }

  // previsit inner decls
  for (auto& inner_decl : decl->declarations()) {
    switch (inner_decl->kind()) {
      case Declaration::Kind::kClass:
        xylo_unreachable();
        break;

      case Declaration::Kind::kFunction: {
        auto method_decl = inner_decl->As<FunctionDeclaration>();
        auto method_symbol = method_decl->symbol();
        xylo_contract(method_symbol->type() == nullptr);

        auto member_info = class_type->AddMethod(method_symbol->name(), nullptr);
        xylo_contract(member_info != nullptr);

        // method type resolver
        auto resolver = [=, this]() {
          VisitFunctionDeclaration(method_decl);
        };
        auto on_update_type = [=]() {
          xylo_contract(method_symbol->type() != nullptr);
          member_info->set_type(method_symbol->type());
        };

        EntityState state(resolver);
        state.on_update_type.push_back(on_update_type);
        RegisterEntityState(method_decl->symbol(), std::move(state));

        member_info->set_type_resolver(resolver);
      }
    }
  }
}


void Inferencer::PrevisitFunctionDeclaration(FunctionDeclaration* decl) {
  auto resolver = [=, this]() {
    VisitFunctionDeclaration(decl);
  };
  RegisterEntityState(decl->symbol(), EntityState(resolver));
}


void Inferencer::VisitDeclaration(Declaration* decl) {
  switch (decl->kind()) {
    case Declaration::Kind::kClass:
      VisitClassDeclaration(decl->As<ClassDeclaration>());
      break;

    case Declaration::Kind::kFunction:
      VisitFunctionDeclaration(decl->As<FunctionDeclaration>());
      break;
  }
}


void Inferencer::VisitClassDeclaration(ClassDeclaration* decl) {
  for (auto& inner_decl : decl->declarations()) {
    if (inner_decl->kind() == Declaration::Kind::kClass) {
      PreVisitClassRegistration(inner_decl->As<ClassDeclaration>());
    }
  }

  for (auto& inner_decl : decl->declarations()) {
    // methods is already previsited in PrevisitClassDeclaration
    if (inner_decl->kind() == Declaration::Kind::kClass) {
      PrevisitDeclaration(inner_decl.get());
    }
  }

  for (auto& inner_decl : decl->declarations()) {
    VisitDeclaration(inner_decl.get());
  }
}


void Inferencer::VisitFunctionDeclaration(FunctionDeclaration* decl) {
  if (decl->symbol()->type() != nullptr) {
    // already processed
    return;
  }

  auto& state = GetEntityState(decl->symbol());
  if (state.in_progress) {
    // recursive call
    auto func_type = decl->func()->type();
    xylo_contract(func_type != nullptr);

    if (func_type->IsGroundType()) {
      decl->symbol()->set_type(func_type);
    } else {
      ReportError(decl->symbol()->position(), "recursive functions must have an explicit type annotation");
      decl->symbol()->set_type(context_->error_type());
    }

    for (auto& callback : state.on_update_type) {
      callback();
    }
    return;
  }

  state.in_progress = true;

  VisitFunctionPrototype(decl->func(), false);
  VisitFunctionBody(decl->func());

  // close over metavariables
  auto depth = decl->func()->scope()->depth();
  auto func_type = decl->func()->type();
  func_type = func_type->CloseOverMetavars(depth, decl->func());
  func_type->PruneInnerScopeVars(depth);
  decl->func()->set_type(func_type);

  // generalize function type and re-set it to symbol
  auto generalized_type = func_type->Generalize(depth, decl->func());
  decl->symbol()->set_type(generalized_type);

  {
    auto& state = GetEntityState(decl->symbol());
    state.in_progress = false;

    // notify completion
    for (auto& callback : state.on_update_type) {
      callback();
    }
  }
}


void Inferencer::VisitBlock(Block* block, InferenceContext* ctx) {
  for (auto& decl : block->declarations()) {
    if (decl->kind() == Declaration::Kind::kClass) {
      PreVisitClassRegistration(decl->As<ClassDeclaration>());
    }
  }

  for (auto& decl : block->declarations()) {
    PrevisitDeclaration(decl.get());
  }

  for (auto& decl : block->declarations()) {
    VisitDeclaration(decl.get());
  }

  InferenceContext local_ctx(*ctx);
  local_ctx.needs_value = false;

  size_t i = 0;
  for (auto& stmt : block->statements()) {
    if (i + 1 == block->statements().size()) {
      local_ctx.needs_value = ctx->needs_value;
    }

    VisitStatement(stmt.get(), &local_ctx);
    ++i;
  }
}


void Inferencer::VisitStatement(Statement* stmt, InferenceContext* ctx) {
  switch (stmt->kind()) {
    case Statement::Kind::kExpression:
      VisitExpressionStatement(stmt->As<ExpressionStatement>(), ctx);
      break;

    case Statement::Kind::kLet:
      VisitLetStatement(stmt->As<LetStatement>(), ctx);
      break;

    case Statement::Kind::kVar:
      VisitVarStatement(stmt->As<VarStatement>(), ctx);
      break;

    case Statement::Kind::kReturn:
      VisitReturnStatement(stmt->As<ReturnStatement>(), ctx);
      break;
  }
}


void Inferencer::VisitExpressionStatement(ExpressionStatement* stmt, InferenceContext* ctx) {
  VisitExpression(stmt->expr(), ctx);
}


void Inferencer::VisitLetStatement(LetStatement* stmt, InferenceContext* ctx) {
  xylo_contract(stmt->symbol()->type() == nullptr);

  InferenceContext local_ctx(*ctx);
  local_ctx.needs_value = true;
  VisitExpression(stmt->expr(), &local_ctx);

  stmt->symbol()->set_type(stmt->expr()->type());
}


void Inferencer::VisitVarStatement(VarStatement* stmt, InferenceContext* ctx) {
  xylo_contract(stmt->symbol()->type() == nullptr);

  // constraints may be added by later assignment expressions
  auto var_type = new TypeMetavar();
  stmt->adopt_type(TypePtr(var_type));
  stmt->symbol()->set_type(var_type);

  InferenceContext local_ctx(*ctx);
  local_ctx.needs_value = true;
  VisitExpression(stmt->expr(), &local_ctx);

  stmt->expr()->type()->ConstrainSubtypeOf(var_type);
}


void Inferencer::VisitReturnStatement(ReturnStatement* stmt, InferenceContext* ctx) {
  InferenceContext local_ctx(*ctx);
  local_ctx.needs_value = true;
  VisitExpression(stmt->expr(), &local_ctx);

  auto return_type = ctx->enclosing_func->type()->As<FunctionType>()->return_type();
  if (!stmt->expr()->type()->ConstrainSubtypeOf(return_type)) {
    ReportError(stmt->position(), "incompatible return type");
  }
}


void Inferencer::VisitExpression(Expression* expr, InferenceContext* ctx) {
  switch (expr->kind()) {
    case Expression::Kind::kNull:
      VisitNullExpression(expr->As<NullExpression>(), ctx);
      break;

    case Expression::Kind::kLiteral:
      VisitLiteralExpression(expr->As<LiteralExpression>(), ctx);
      break;

    case Expression::Kind::kThis:
      VisitThisExpression(expr->As<ThisExpression>(), ctx);
      break;

    case Expression::Kind::kIdentifier:
      VisitIdentifierExpression(expr->As<IdentifierExpression>(), ctx);
      break;

    case Expression::Kind::kFunction:
      VisitFunctionExpression(expr->As<FunctionExpression>(), ctx);
      break;

    case Expression::Kind::kTuple:
      VisitTupleExpression(expr->As<TupleExpression>(), ctx);
      break;

    case Expression::Kind::kApply:
      VisitApplyExpression(expr->As<ApplyExpression>(), ctx);
      break;

    case Expression::Kind::kUnary:
      VisitUnaryExpression(expr->As<UnaryExpression>(), ctx);
      break;

    case Expression::Kind::kBinary:
      VisitBinaryExpression(expr->As<BinaryExpression>(), ctx);
      break;

    case Expression::Kind::kConditional:
      VisitConditionalExpression(expr->As<ConditionalExpression>(), ctx);
      break;

    case Expression::Kind::kNew:
      VisitNewExpression(expr->As<NewExpression>(), ctx);
      break;

    case Expression::Kind::kProjection:
      VisitProjectionExpression(expr->As<ProjectionExpression>(), ctx);
      break;

    case Expression::Kind::kBlock:
      VisitBlockExpression(expr->As<BlockExpression>(), ctx);
      break;
  }
}


void Inferencer::VisitNullExpression(NullExpression* expr, InferenceContext* ctx) {
  xylo_contract(expr->type() == nullptr);
  expr->set_type(context_->unit_type());
}


void Inferencer::VisitLiteralExpression(LiteralExpression* expr, InferenceContext* ctx) {
  xylo_contract(expr->type() != nullptr);
}


void Inferencer::VisitThisExpression(ThisExpression* expr, InferenceContext* ctx) {
  xylo_contract(expr->type() == nullptr);
  xylo_contract(expr->class_symbol() != nullptr);

  auto class_type = expr->class_symbol()->type();
  xylo_contract(class_type != nullptr);

  expr->set_type(class_type);
}


void Inferencer::VisitIdentifierExpression(IdentifierExpression* expr, InferenceContext* ctx) {
  xylo_contract(expr->type() == nullptr);
  xylo_contract(expr->symbol() != nullptr);

  auto symbol = expr->symbol();
  auto symbol_type = symbol->type();

  switch (symbol->kind()) {
    case Symbol::Kind::kLetVariable:
    case Symbol::Kind::kVarVariable: {
      xylo_contract(symbol_type != nullptr);
      expr->set_type(symbol_type);
      break;
    }

    case Symbol::Kind::kFunction: {
      auto& state = GetEntityState(expr->symbol());

      if (symbol_type == nullptr) {
        state.resolve();
        symbol_type = expr->symbol()->type();
        xylo_contract(symbol_type != nullptr);
      }

      Vector<TypeMetavar*> instantiated_vars;
      auto expr_type = symbol_type->Instantiate(expr, &instantiated_vars);
      expr->set_type(expr_type);
      expr->set_instantiated_vars(std::move(instantiated_vars));
      break;
    }

    default:
      xylo_unreachable();
      break;
  }
}


void Inferencer::VisitTupleExpression(TupleExpression* expr, InferenceContext* ctx) {
  xylo_contract(expr->type() == nullptr);

  auto tuple_type = new TupleType();
  expr->adopt_type(TypePtr(tuple_type));

  for (auto& element : expr->elements()) {
    VisitExpression(element.get(), ctx);
    tuple_type->add_element(element->type());
  }

  expr->set_type(tuple_type);
}


void Inferencer::VisitFunctionExpression(FunctionExpression* expr, InferenceContext* ctx) {
  VisitFunctionPrototype(expr, true);
  VisitFunctionBody(expr);
}


void Inferencer::VisitFunctionPrototype(FunctionExpression* expr, bool is_lambda) {
  xylo_contract(expr->type() == nullptr);

  auto param_type = new TupleType();
  expr->adopt_type(TypePtr(param_type));

  // set up parameter types
  for (auto& param : expr->params()) {
    auto symbol = param->symbol();
    xylo_contract(symbol->type() == nullptr);

    if (param->type_repr() != nullptr) {
      auto declared_type = ConvertDeclarableTypeRepr(param->type_repr(), expr);
      symbol->set_type(declared_type);
    } else {
      Type* var;
      if (is_lambda) {
        var = new TypeMetavar();
      } else {
        var = new TypeVariable(expr->scope()->depth());
      }
      expr->adopt_type(TypePtr(var));
      symbol->set_type(var);
    }

    param_type->add_element(symbol->type());
  }

  Type* return_type;
  if (expr->return_type_repr() != nullptr) {
    return_type = ConvertDeclarableTypeRepr(expr->return_type_repr(), expr);
  } else {
    return_type = new TypeMetavar();
    expr->adopt_type(TypePtr(return_type));
  }

  auto func_type = new FunctionType(expr->is_closure(), param_type, return_type);
  expr->adopt_type(TypePtr(func_type));
  expr->set_type(func_type);
}


void Inferencer::VisitFunctionBody(FunctionExpression* expr) {
  xylo_contract(expr->type() != nullptr);

  InferenceContext local_ctx{.enclosing_func = expr, .needs_value = false};
  VisitBlock(expr->body(), &local_ctx);

  // when not assigned to return type, set it to unit type
  auto return_type = expr->type()->As<FunctionType>()->return_type();
  if (return_type->kind() == Type::Kind::kMetavar) {
    auto ret_metavar = return_type->As<TypeMetavar>();
    if (ret_metavar->lower_bound()->empty()) {
      context_->unit_type()->ConstrainSubtypeOf(return_type);
    }
  }
}


void Inferencer::VisitApplyExpression(ApplyExpression* expr, InferenceContext* ctx) {
  xylo_contract(expr->type() == nullptr);

  InferenceContext local_ctx(*ctx);
  local_ctx.needs_value = true;
  VisitExpression(expr->func(), &local_ctx);
  VisitExpression(expr->args(), &local_ctx);

  auto return_type = new TypeMetavar();
  expr->set_type(return_type);
  expr->adopt_type(TypePtr(return_type));

  // accept regular functions and closures
  auto constraint = new FunctionType(true, expr->args()->type()->As<TupleType>(), return_type);
  expr->adopt_type(TypePtr(constraint));

  auto func_type = expr->func()->type();
  if (!func_type->ConstrainSubtypeOf(constraint)) {
    if (!func_type->is_function_type()) {
      if (expr->func()->kind() == Expression::Kind::kIdentifier) {
        auto ident_expr = expr->func()->As<IdentifierExpression>();
        auto ident_name = ident_expr->symbol()->name();
        ReportError(expr->func()->position(), "value '" + ident_name->str().cpp_str() + "' is not callable");
      } else {
        ReportError(expr->position(), "expression is not callable");
      }
    } else {
      ReportError(expr->position(), "function call has incompatible argument types");
    }
    expr->set_type(context_->error_type());
  }
}


void Inferencer::VisitUnaryExpression(UnaryExpression* expr, InferenceContext* ctx) {
  xylo_contract(expr->type() == nullptr);

  InferenceContext local_ctx(*ctx);
  local_ctx.needs_value = true;
  VisitExpression(expr->operand(), &local_ctx);

  auto error = [=, this](const char* msg) {
    std::ostringstream oss;
    oss << "operator " << TokenName(expr->op()) << ": " << msg;
    ReportError(expr->position(), oss.str());
  };

  switch (expr->op()) {
    case Token::kAdd:
    case Token::kSub: {
      auto operand_type = expr->operand()->type();
      if (!operand_type->ConstrainSubtypeOf(context_->numeric_type())) {
        error("operand must have numeric type");
      }
      expr->set_type(operand_type);
      break;
    }

    case Token::kNot: {
      auto operand_type = expr->operand()->type();
      if (!operand_type->ConstrainSubtypeOf(context_->int_type())) {
        error("operand must have integral type");
      }
      expr->set_type(operand_type);
      break;
    }

    case Token::kLNot: {
      auto operand_type = expr->operand()->type();
      if (!operand_type->ConstrainSubtypeOf(context_->bool_type())) {
        error("operand must have boolean type");
      }
      expr->set_type(operand_type);
      break;
    }

    default:
      xylo_unreachable();
      break;
  }
}


void Inferencer::VisitBinaryExpression(BinaryExpression* expr, InferenceContext* ctx) {
  xylo_contract(expr->type() == nullptr);

  auto error = [=, this](const char* msg) {
    std::ostringstream oss;
    oss << "operator " << TokenName(expr->op()) << ": " << msg;
    ReportError(expr->position(), oss.str());
  };

  InferenceContext local_ctx(*ctx);
  local_ctx.needs_value = true;
  VisitExpression(expr->lhs(), &local_ctx);
  VisitExpression(expr->rhs(), &local_ctx);

  auto lhs_type = expr->lhs()->type();
  auto rhs_type = expr->rhs()->type();

  // check assignment operand
  switch (expr->op()) {
    case Token::kAssign:
    case Token::kAddAssign:
    case Token::kSubAssign:
    case Token::kMulAssign:
    case Token::kDivAssign:
    case Token::kRemAssign:
    case Token::kAndAssign:
    case Token::kOrAssign:
    case Token::kXorAssign:
    case Token::kShlAssign:
    case Token::kShrAssign: {
      if (!MarkLValue(expr->lhs())) {
        ReportError(expr->position(), "cannot assign to left operand");
        expr->set_type(lhs_type);
        return;
      }
      break;
    }
    default:
      break;
  }

  switch (expr->op()) {
    case Token::kAdd:
    case Token::kSub:
    case Token::kMul:
    case Token::kDiv:
    case Token::kRem: {
      // both sides must have numeric types
      if (!lhs_type->ConstrainSubtypeOf(context_->numeric_type())) {
        error("left operand must have numeric type");
      }
      if (!rhs_type->ConstrainSubtypeOf(context_->numeric_type())) {
        error("right operand must have numeric type");
      }

      auto expr_type = new TypeMetavar();
      expr->set_type(expr_type);
      expr->adopt_type(TypePtr(expr_type));

      // take a higher numeric type as result type
      lhs_type->ConstrainSubtypeOf(expr_type);
      rhs_type->ConstrainSubtypeOf(expr_type);

      break;
    }

    case Token::kAddAssign:
    case Token::kSubAssign:
    case Token::kMulAssign:
    case Token::kDivAssign:
    case Token::kRemAssign: {
      // both sides must have numeric types
      if (!lhs_type->ConstrainSubtypeOf(context_->numeric_type())) {
        error("left operand must have numeric type");
      }
      if (!rhs_type->ConstrainSubtypeOf(context_->numeric_type())) {
        error("right operand must have numeric type");
      }

      // rhs must be subtype of lhs
      if (!rhs_type->ConstrainSubtypeOf(lhs_type)) {
        error("operand types are incompatible");
      }

      expr->set_type(lhs_type);
      break;
    }

    case Token::kAssign: {
      // rhs must be subtype to lhs
      if (!rhs_type->ConstrainSubtypeOf(lhs_type)) {
        error("operand types are incompatible");
      }

      expr->set_type(lhs_type);
      break;
    }

    case Token::kAnd:
    case Token::kOr:
    case Token::kXor: {
      // both sides must have integral types
      if (!lhs_type->ConstrainSubtypeOf(context_->int_type())) {
        error("left operand must have integral type");
      }
      if (!rhs_type->ConstrainSubtypeOf(context_->int_type())) {
        error("right operand must have integral type");
      }

      // lhs and rhs must have the same type
      if (!lhs_type->ConstrainSameAs(rhs_type)) {
        error("both sides must have the same type");
      }

      auto expr_type = new TypeMetavar();
      expr->set_type(expr_type);
      expr->adopt_type(TypePtr(expr_type));

      // take a numeric type as result type
      lhs_type->ConstrainSubtypeOf(expr_type);
      rhs_type->ConstrainSubtypeOf(expr_type);

      break;
    }

    case Token::kAndAssign:
    case Token::kOrAssign:
    case Token::kXorAssign: {
      // both sides must have integral types
      if (!lhs_type->ConstrainSubtypeOf(context_->int_type())) {
        error("left operand must have integral type");
      }
      if (!rhs_type->ConstrainSubtypeOf(context_->int_type())) {
        error("right operand must have integral type");
      }

      // lhs and rhs must have the same type
      if (!lhs_type->ConstrainSameAs(rhs_type)) {
        error("both sides must have the same type");
      }

      expr->set_type(lhs_type);
      break;
    }

    case Token::kShl:
    case Token::kShr:
    case Token::kShlAssign:
    case Token::kShrAssign: {
      // both sides must have integral types
      if (!lhs_type->ConstrainSubtypeOf(context_->int_type())) {
        error("left operand must have integral type");
      }
      if (!rhs_type->ConstrainSubtypeOf(context_->int_type())) {
        error("right operand must have integral type");
      }

      // take lhs type as result type
      expr->set_type(lhs_type);
      break;
    }

    case Token::kLAnd:
    case Token::kLOr: {
      // both sides must have boolean types
      if (!lhs_type->ConstrainSubtypeOf(context_->bool_type())) {
        error("left operand must have boolean type");
      }
      if (!rhs_type->ConstrainSubtypeOf(context_->bool_type())) {
        error("right operand must have boolean type");
      }

      expr->set_type(context_->bool_type());
      break;
    }

    case Token::kEQ:
    case Token::kNE: {
      // both sides must have same types
      if (!lhs_type->ConstrainSameAs(rhs_type)) {
        error("both sides must have the same type");
      }

      expr->set_type(context_->bool_type());
      break;
    }

    case Token::kLT:
    case Token::kLE:
    case Token::kGT:
    case Token::kGE: {
      // both sides must have comparable types
      if (!lhs_type->ConstrainSubtypeOf(context_->comparable_type())) {
        error("left operand must have comparable type");
      }
      if (!rhs_type->ConstrainSubtypeOf(context_->comparable_type())) {
        error("right operand must have comparable type");
      }

      // both sides must have same types
      if (!lhs_type->ConstrainSameAs(rhs_type)) {
        error("both sides must have the same type");
      }

      expr->set_type(context_->bool_type());
      break;
    }

    default:
      xylo_unreachable();
      break;
  }
}


void Inferencer::VisitConditionalExpression(ConditionalExpression* expr, InferenceContext* ctx) {
  xylo_contract(expr->type() == nullptr);

  InferenceContext cond_ctx(*ctx);
  cond_ctx.needs_value = true;
  VisitExpression(expr->cond(), &cond_ctx);
  VisitExpression(expr->then_expr(), ctx);
  VisitExpression(expr->else_expr(), ctx);

  // condition must have boolean type
  if (!expr->cond()->type()->ConstrainSubtypeOf(context_->bool_type())) {
    ReportError(expr->cond()->position(), "condition must have boolean type");
  }

  auto then_type = expr->then_expr()->type();
  auto else_type = expr->else_expr()->type();

  // if the value is not needed, set the type to 'unit'
  if (!ctx->needs_value) {
    expr->set_type(context_->unit_type());
    return;
  }

  auto expr_type = new TypeMetavar();
  expr->set_type(expr_type);
  expr->adopt_type(TypePtr(expr_type));

  if (expr->else_expr()->kind() == Expression::Kind::kNull) {
    ReportError(expr->position(), "missing 'else' branch");
    return;
  }

  // then branch and else branch must be compatible with expression type
  if (!then_type->ConstrainSubtypeOf(expr_type)) {
    xylo_unreachable();  // always succeeds for fresh metavars
  }
  if (!else_type->ConstrainSubtypeOf(expr_type)) {
    ReportError(expr->position(), "incompatible types in conditional branches");
  }
}


void Inferencer::VisitNewExpression(NewExpression* expr, InferenceContext* ctx) {
  xylo_contract(expr->type() == nullptr);
  xylo_contract(expr->class_symbol() != nullptr);

  auto symbol = expr->class_symbol();
  auto class_type = symbol->type();
  xylo_contract(class_type != nullptr);
  auto nominal_type = class_type->As<NominalType>();
  expr->set_type(class_type);

  // for checking all fields are initialized
  Set<Identifier*> remain_fields;
  for (auto& field : nominal_type->fields()) {
    remain_fields.emplace(field->name());
  }

  InferenceContext local_ctx(*ctx);
  local_ctx.needs_value = true;

  // initializers
  for (auto& init : expr->field_initializers()) {
    VisitExpression(init->expr(), &local_ctx);
    xylo_contract(init->expr()->type() != nullptr);

    auto field_info = nominal_type->GetMember(init->field_name());
    if (field_info == nullptr) {
      ReportError(init->position(), "unknown field '" + init->field_name()->str().cpp_str() + "'");
      continue;
    }
    if (!init->expr()->type()->ConstrainSubtypeOf(field_info->type())) {
      ReportError(init->position(), "field '" + init->field_name()->str().cpp_str() + "' has incompatible type");
    }

    remain_fields.erase(init->field_name());
  }

  for (auto& field_name : remain_fields) {
    ReportError(expr->position(), "missing field '" + field_name->str().cpp_str() + "'");
  }
}


void Inferencer::VisitProjectionExpression(ProjectionExpression* expr, InferenceContext* ctx) {
  xylo_contract(expr->type() == nullptr);

  InferenceContext local_ctx(*ctx);
  local_ctx.needs_value = true;
  VisitExpression(expr->object(), &local_ctx);

  auto object_type = expr->object()->type();
  xylo_contract(object_type != nullptr);

  auto member_type = new TypeMetavar();
  auto memreq = new MemberRequirement(expr->member_name(), member_type);
  expr->adopt_type(TypePtr(member_type));
  expr->adopt_type(TypePtr(memreq));
  expr->set_type(member_type);
  expr->set_member_req(memreq);

  memreq->set_on_error([this, expr](const NominalType* nominal) {
    auto member_info = nominal->GetMember(expr->member_name());
    if (member_info == nullptr) {
      ReportError(expr->position(), "cannot find member '" + expr->member_name()->str().cpp_str() + "'");
    } else {
      ReportError(expr->position(), "member '" + expr->member_name()->str().cpp_str() + "' has incompatible type");
    }
  });

  object_type->ConstrainSubtypeOf(memreq);
}


void Inferencer::VisitBlockExpression(BlockExpression* expr, InferenceContext* ctx) {
  xylo_contract(expr->type() == nullptr);

  VisitBlock(expr->block(), ctx);

  if (!ctx->needs_value) {
    // block value not needed, set to unit type
    expr->set_type(context_->unit_type());
    return;
  }

  // determine block expression type from last statement
  if (expr->block()->statements().empty()) {
    ReportError(expr->position(), "block is empty");
    expr->set_type(context_->error_type());
    return;
  }

  auto last_stmt = expr->block()->statements().back().get();
  switch (last_stmt->kind()) {
    case Statement::Kind::kExpression: {
      auto expr_stmt = last_stmt->As<ExpressionStatement>();
      expr->set_type(expr_stmt->expr()->type());
      return;
    }

    case Statement::Kind::kReturn: {
      auto bottom_type = new UnionType();
      expr->set_type(bottom_type);  // a.k.a. 'never' type
      expr->adopt_type(TypePtr(bottom_type));
      return;
    }

    case Statement::Kind::kLet:
    case Statement::Kind::kVar:
      ReportError(last_stmt->position(), "block must end with an expression");
      expr->set_type(context_->error_type());
      return;
  }
}


Type* Inferencer::ConvertDeclarableTypeRepr(TypeRepr* type_repr, TypeSink* allocated) {
  auto type = ConvertTypeRepr(type_repr, allocated);

  switch (type->kind()) {
    case Type::Kind::kNominal:
    case Type::Kind::kFunction:
      return type;

    default: {
      ReportError(type_repr->position(), "unsupported type in declaration");
      auto metavar = new TypeMetavar();
      allocated->adopt_type(TypePtr(metavar));
      return metavar;
    }
  }
}


Type* Inferencer::ConvertTypeRepr(TypeRepr* type_repr, TypeSink* allocated) {
  switch (type_repr->kind()) {
    case TypeRepr::Kind::kNull:
      xylo_unreachable();

    case TypeRepr::Kind::kNamed:
      return ConvertNamedTypeRepr(type_repr->As<NamedTypeRepr>(), allocated);

    case TypeRepr::Kind::kTuple:
      return ConvertTupleTypeRepr(type_repr->As<TupleTypeRepr>(), allocated);

    case TypeRepr::Kind::kFunction:
      return ConvertFunctionTypeRepr(type_repr->As<FunctionTypeRepr>(), allocated);
  }

  xylo_unreachable();
}


Type* Inferencer::ConvertNamedTypeRepr(NamedTypeRepr* type_repr, TypeSink* allocated) {
  return type_repr->symbol()->type();
}


FunctionType* Inferencer::ConvertFunctionTypeRepr(FunctionTypeRepr* type_repr, TypeSink* allocated) {
  auto params_type = ConvertTupleTypeRepr(type_repr->params_type(), allocated);
  auto return_type = ConvertDeclarableTypeRepr(type_repr->return_type(), allocated);
  auto function_type = new FunctionType(true, params_type, return_type);
  allocated->adopt_type(TypePtr(function_type));
  return function_type;
}


TupleType* Inferencer::ConvertTupleTypeRepr(TupleTypeRepr* type_repr, TypeSink* allocated) {
  auto tuple_type = new TupleType();
  for (auto& element : type_repr->elements()) {
    auto element_type = ConvertDeclarableTypeRepr(element.get(), allocated);
    tuple_type->add_element(element_type);
  }
  allocated->adopt_type(TypePtr(tuple_type));
  return tuple_type;
}


Inferencer::EntityState& Inferencer::GetEntityState(Symbol* symbol) {
  auto it = entity_states_.find(symbol);
  xylo_contract(it != entity_states_.end());
  return it->second;
}


void Inferencer::RegisterEntityState(Symbol* symbol, EntityState&& state) {
  auto result = entity_states_.emplace(symbol, std::move(state));
  xylo_contract(result.second);
}


bool Inferencer::MarkLValue(Expression* expr) {
  switch (expr->kind()) {
    case Expression::Kind::kIdentifier: {
      auto ident_expr = expr->As<IdentifierExpression>();
      auto symbol = ident_expr->symbol();
      if (symbol->kind() == Symbol::Kind::kVarVariable) {
        ident_expr->set_lvalue(true);
        return true;
      } else {
        return false;
      }
    }

    case Expression::Kind::kProjection: {
      auto proj_expr = expr->As<ProjectionExpression>();
      auto member_req = proj_expr->member_req();
      if (member_req == nullptr) {
        return true;  // error case, skip
      } else if (member_req->SetMutable(true)) {
        proj_expr->set_lvalue(true);
        return true;
      } else {
        return false;
      }
    }

    default:
      return false;
  }
}


}  // namespace xylo
