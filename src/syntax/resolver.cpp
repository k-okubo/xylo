
#include "xylo/syntax/resolver.h"

namespace xylo {


void Resolver::VisitFileAST(FileAST* file_ast, const NameTable* external_name_table) {
  NameTable global_table(external_name_table);
  ResolutionContext global_ctx = {&global_table, {nullptr, nullptr, nullptr}};

  // TODO: handle external functions
  Vector<FunctionExpressionPtr> external_functions;
  if (external_name_table != nullptr) {
    for (auto [_, symbol] : external_name_table->entries()) {
      if (symbol->kind() == Symbol::Kind::kFunction) {
        auto func = FunctionExpression::Create({}, Block::Create());
        RegisterDecledFunction(symbol, func.get());
        external_functions.push_back(std::move(func));
      }
    }
  }

  // TODO: primitive types
  auto insert_global = [&](Symbol* symbol) {
    global_table.Insert(symbol->name(), symbol);
  };
  insert_global(context_->unit_symbol());
  insert_global(context_->bool_symbol());
  insert_global(context_->comparable_symbol());
  insert_global(context_->numeric_symbol());
  insert_global(context_->float_symbol());
  insert_global(context_->int_symbol());
  insert_global(context_->string_symbol());

  for (auto& decl : file_ast->declarations()) {
    PrevisitDeclaration(decl.get(), &global_ctx);
  }

  for (auto& decl : file_ast->declarations()) {
    VisitDeclaration(decl.get(), &global_ctx);
  }
}


void Resolver::PrevisitDeclaration(Declaration* decl, ResolutionContext* ctx) {
  auto symbol = decl->symbol();
  auto name = symbol->name();

  if (!ctx->name_table->Insert(name, symbol)) {
    ErrorRedefinition(symbol->position(), name, ctx->name_table->Lookup(name));
  }

  switch (decl->kind()) {
    case Declaration::Kind::kInterface:
      break;

    case Declaration::Kind::kClass: {
      auto class_decl = decl->As<ClassDeclaration>();
      RegisterDecledClass(symbol, class_decl);
      break;
    }

    case Declaration::Kind::kFunction: {
      auto func_decl = decl->As<FunctionDeclaration>();
      RegisterDecledFunction(symbol, func_decl->func());
      break;
    }
  }
}


void Resolver::VisitDeclaration(Declaration* decl, ResolutionContext* ctx) {
  switch (decl->kind()) {
    case Declaration::Kind::kInterface:
      VisitInterfaceDeclaration(decl->As<InterfaceDeclaration>(), ctx);
      break;

    case Declaration::Kind::kClass:
      VisitClassDeclaration(decl->As<ClassDeclaration>(), ctx);
      break;

    case Declaration::Kind::kFunction:
      VisitFunctionDeclaration(decl->As<FunctionDeclaration>(), ctx);
      break;
  }
}


void Resolver::VisitInterfaceDeclaration(InterfaceDeclaration* decl, ResolutionContext* ctx) {
  for (auto& super : decl->supers()) {
    VisitSuperClass(super.get(), ctx);
  }

  NameTable local_table(ctx->name_table);
  ResolutionContext local_ctx = {&local_table, ctx->enclosure};
  local_ctx.enclosure.interface = decl;

  for (auto& method : decl->methods()) {
    PrevisitDeclaration(method.get(), &local_ctx);
  }

  for (auto& method : decl->methods()) {
    VisitDeclaration(method.get(), &local_ctx);
  }
}


void Resolver::VisitClassDeclaration(ClassDeclaration* decl, ResolutionContext* ctx) {
  for (auto& super : decl->supers()) {
    VisitSuperClass(super.get(), ctx);
  }

  for (auto& embedded : decl->embeddeds()) {
    VisitEmbeddedClass(embedded.get(), decl, ctx);
  }

  NameTable local_table(ctx->name_table);
  ResolutionContext local_ctx = {&local_table, ctx->enclosure};
  local_ctx.enclosure.clazz = decl;

  for (auto& field : decl->fields()) {
    VisitClassField(field.get(), &local_ctx);
  }

  for (auto& decl : decl->declarations()) {
    PrevisitDeclaration(decl.get(), &local_ctx);
  }

  for (auto& decl : decl->declarations()) {
    VisitDeclaration(decl.get(), &local_ctx);
  }

  auto enclosing_class = ctx->enclosure.clazz;
  auto enclosing_func = ctx->enclosure.func;
  OnUpdateReferenceScope(decl, [=, this](const EntityState& state) {
    if (enclosing_class != nullptr) {
      MarkAsClosureIfNeeded(enclosing_class, state.reference_scope);
    }
    if (enclosing_func != nullptr) {
      MarkAsClosureIfNeeded(enclosing_func, state.reference_scope);
    }
    if (decl->is_closure() && enclosing_func != nullptr) {
      enclosing_func->set_has_closure(true);
    }
  });
}


void Resolver::VisitClassField(ClassField* field, ResolutionContext* ctx) {
  auto field_symbol = field->symbol();
  auto field_name = field_symbol->name();

  if (!ctx->name_table->Insert(field_name, field_symbol)) {
    ErrorRedefinition(field_symbol->position(), field_name, ctx->name_table->Lookup(field_name));
  }

  if (field->type_repr() != nullptr) {
    VisitTypeRepr(field->type_repr(), ctx);
  }
}


void Resolver::VisitEmbeddedClass(EmbeddedClass* embedded, ClassDeclaration* embedding, ResolutionContext* ctx) {
  auto symbol = ctx->name_table->Lookup(embedded->name());

  if (symbol == nullptr) {
    ErrorUndeclared(embedded->position(), embedded->name());
  } else if (symbol->kind() != Symbol::Kind::kClass) {
    ErrorValueAsType(embedded->position(), symbol);
  } else {
    embedded->set_symbol(symbol);

    auto embedded_decl = GetClass(symbol);
    OnUpdateReferenceScope(embedded_decl, [=, this](const EntityState& state) {
      MarkAsClosureIfNeeded(embedding, state.reference_scope);
    });
  }
}


void Resolver::VisitSuperClass(SuperClass* super, ResolutionContext* ctx) {
  auto symbol = ctx->name_table->Lookup(super->name());

  if (symbol == nullptr) {
    ErrorUndeclared(super->position(), super->name());
  } else if (symbol->kind() != Symbol::Kind::kClass) {
    ErrorValueAsType(super->position(), symbol);
  } else {
    super->set_symbol(symbol);
  }
}


void Resolver::VisitFunctionDeclaration(FunctionDeclaration* decl, ResolutionContext* ctx) {
  VisitFunctionExpression(decl->func(), ctx, false);
}


void Resolver::VisitBlock(Block* block, ResolutionContext* ctx) {
  for (auto& decl : block->declarations()) {
    PrevisitDeclaration(decl.get(), ctx);
  }

  for (auto& decl : block->declarations()) {
    VisitDeclaration(decl.get(), ctx);
  }

  for (auto& stmt : block->statements()) {
    VisitStatement(stmt.get(), ctx);
  }

  CollectCapturedSymbols(ctx->enclosure.func, block);
}


void Resolver::VisitStatement(Statement* stmt, ResolutionContext* ctx) {
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


void Resolver::VisitExpressionStatement(ExpressionStatement* stmt, ResolutionContext* ctx) {
  VisitExpression(stmt->expr(), ctx);
}


void Resolver::VisitLetStatement(LetStatement* stmt, ResolutionContext* ctx) {
  VisitExpression(stmt->expr(), ctx);

  auto symbol = stmt->symbol();
  auto name = symbol->name();

  if (!ctx->name_table->Insert(name, symbol)) {
    ErrorRedefinition(symbol->position(), name, ctx->name_table->Lookup(name));
  }
}


void Resolver::VisitVarStatement(VarStatement* stmt, ResolutionContext* ctx) {
  VisitExpression(stmt->expr(), ctx);

  auto symbol = stmt->symbol();
  auto name = symbol->name();

  if (!ctx->name_table->Insert(name, symbol)) {
    ErrorRedefinition(symbol->position(), name, ctx->name_table->Lookup(name));
  }
}


void Resolver::VisitReturnStatement(ReturnStatement* stmt, ResolutionContext* ctx) {
  VisitExpression(stmt->expr(), ctx);
}


void Resolver::VisitExpression(Expression* expr, ResolutionContext* ctx) {
  switch (expr->kind()) {
    case Expression::Kind::kNull:
      break;

    case Expression::Kind::kLiteral:
      break;

    case Expression::Kind::kThis:
      VisitThisExpression(expr->As<ThisExpression>(), ctx);
      break;

    case Expression::Kind::kIdentifier:
      VisitIdentifierExpression(expr->As<IdentifierExpression>(), ctx);
      break;

    case Expression::Kind::kTuple:
      VisitTupleExpression(expr->As<TupleExpression>(), ctx);
      break;

    case Expression::Kind::kFunction:
      VisitFunctionExpression(expr->As<FunctionExpression>(), ctx, true);
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

    case Expression::Kind::kConstruct:
      VisitConstructExpression(expr->As<ConstructExpression>(), ctx);
      break;

    case Expression::Kind::kSelect:
      VisitSelectExpression(expr->As<SelectExpression>(), ctx);
      break;

    case Expression::Kind::kBlock:
      VisitBlockExpression(expr->As<BlockExpression>(), ctx);
      break;
  }
}


void Resolver::VisitThisExpression(ThisExpression* expr, ResolutionContext* ctx) {
  if (ctx->enclosure.clazz == nullptr) {
    ReportError(expr->position(), "invalid use of 'this' outside of class contexts");
  } else {
    expr->set_class_symbol(ctx->enclosure.clazz->symbol());
  }
}


void Resolver::VisitIdentifierExpression(IdentifierExpression* expr, ResolutionContext* ctx) {
  auto symbol = ctx->name_table->Lookup(expr->name());

  if (symbol == nullptr) {
    ErrorUndeclared(expr->position(), expr->name());
  } else if (symbol->kind() == Symbol::Kind::kClass) {
    ErrorTypeAsValue(expr->position(), symbol);
  } else {
    // LetVariable or VarVariable or Function
    expr->set_symbol(symbol);

    switch (symbol->kind()) {
      case Symbol::Kind::kClass:
        break;

      case Symbol::Kind::kLetVariable:
      case Symbol::Kind::kVarVariable:
        MarkAsClosureIfNeeded(ctx->enclosure.func, symbol);
        break;

      case Symbol::Kind::kFunction: {
        auto reference_func = GetFunction(symbol);
        auto enclosing_func = ctx->enclosure.func;
        OnUpdateReferenceScope(reference_func, [=, this](const EntityState& state) {
          if (reference_func->is_closure()) {
            MarkAsClosureIfNeeded(enclosing_func, symbol);
          }
        });
        break;
      }
    }
  }
}


void Resolver::VisitTupleExpression(TupleExpression* expr, ResolutionContext* ctx) {
  for (auto& element : expr->elements()) {
    VisitExpression(element.get(), ctx);
  }
}


void Resolver::VisitFunctionExpression(FunctionExpression* expr, ResolutionContext* ctx, bool is_lambda) {
  NameTable local_table(ctx->name_table);
  ResolutionContext local_ctx = {&local_table, ctx->enclosure};
  local_ctx.enclosure.func = expr;

  if (is_lambda) {
    RegisterLambdaFunction(expr);
  }

  for (auto& param : expr->params()) {
    auto symbol = param->symbol();
    auto name = symbol->name();
    if (!local_table.Insert(name, symbol)) {
      ErrorRedefinition(symbol->position(), name, local_table.Lookup(name));
    }
    if (param->type_repr() != nullptr) {
      VisitTypeRepr(param->type_repr(), &local_ctx);
    }
  }

  if (expr->body() != nullptr) {
    VisitBlock(expr->body(), &local_ctx);
  }
  if (expr->return_type_repr() != nullptr) {
    VisitTypeRepr(expr->return_type_repr(), &local_ctx);
  }

  if (ctx->enclosure.interface != nullptr || ctx->enclosure.clazz != nullptr) {
    expr->set_closure(true);
  }

  auto enclosing_class = ctx->enclosure.clazz;
  auto enclosing_func = ctx->enclosure.func;
  OnUpdateReferenceScope(expr, [=, this](const EntityState& state) {
    if (enclosing_class != nullptr) {
      MarkAsClosureIfNeeded(enclosing_class, state.reference_scope);
    }
    if (enclosing_func != nullptr) {
      MarkAsClosureIfNeeded(enclosing_func, state.reference_scope);
    }
    if (expr->is_closure() && enclosing_func != nullptr) {
      enclosing_func->set_has_closure(true);
    }
  });

  CollectCapturedSymbols(expr);
}


void Resolver::VisitApplyExpression(ApplyExpression* expr, ResolutionContext* ctx) {
  VisitExpression(expr->func(), ctx);
  VisitExpression(expr->args(), ctx);
}


void Resolver::VisitUnaryExpression(UnaryExpression* expr, ResolutionContext* ctx) {
  VisitExpression(expr->operand(), ctx);
}


void Resolver::VisitBinaryExpression(BinaryExpression* expr, ResolutionContext* ctx) {
  VisitExpression(expr->lhs(), ctx);
  VisitExpression(expr->rhs(), ctx);
}


void Resolver::VisitConditionalExpression(ConditionalExpression* expr, ResolutionContext* ctx) {
  VisitExpression(expr->cond(), ctx);
  VisitExpression(expr->then_expr(), ctx);
  VisitExpression(expr->else_expr(), ctx);
}


void Resolver::VisitConstructExpression(ConstructExpression* expr, ResolutionContext* ctx) {
  auto class_symbol = ctx->name_table->Lookup(expr->class_name());

  if (class_symbol == nullptr) {
    ErrorUndeclared(expr->position(), expr->class_name());
  } else if (class_symbol->kind() != Symbol::Kind::kClass) {
    ErrorValueAsType(expr->position(), class_symbol);
  } else {
    expr->set_class_symbol(class_symbol);

    auto class_decl = GetClass(class_symbol);
    auto enclosing_func = ctx->enclosure.func;
    OnUpdateReferenceScope(class_decl, [=, this](const EntityState& state) {
      if (class_decl->is_closure()) {
        MarkAsClosureIfNeeded(enclosing_func, class_decl->symbol());
      }
    });
  }

  VisitObjectInitializer(expr->initializer(), ctx);
}


void Resolver::VisitSelectExpression(SelectExpression* expr, ResolutionContext* ctx) {
  VisitExpression(expr->object(), ctx);
}


void Resolver::VisitBlockExpression(BlockExpression* expr, ResolutionContext* ctx) {
  NameTable local_table(ctx->name_table);
  ResolutionContext local_ctx = {&local_table, ctx->enclosure};

  VisitBlock(expr->block(), &local_ctx);
}


void Resolver::VisitExpressionInitializer(ExpressionInitializer* initializer, ResolutionContext* ctx) {
  VisitExpression(initializer->expr(), ctx);
}


void Resolver::VisitObjectInitializer(ObjectInitializer* initializer, ResolutionContext* ctx) {
  Set<Identifier*> initialized_fields;

  for (auto& entry : initializer->entries()) {
    auto [_, success] = initialized_fields.emplace(entry->name());
    if (!success) {
      ReportError(entry->position(), "duplicate field name '" + entry->name()->str().cpp_str() + "'");
    }

    VisitFieldEntry(entry.get(), ctx);
  }
}


void Resolver::VisitFieldEntry(FieldEntry* entry, ResolutionContext* ctx) {
  switch (entry->value()->kind()) {
    case Initializer::Kind::kExpression:
      VisitExpressionInitializer(entry->value()->As<ExpressionInitializer>(), ctx);
      break;

    case Initializer::Kind::kObject:
      VisitObjectInitializer(entry->value()->As<ObjectInitializer>(), ctx);
      break;
  }
}


void Resolver::VisitTypeRepr(TypeRepr* type_repr, ResolutionContext* ctx) {
  switch (type_repr->kind()) {
    case TypeRepr::Kind::kNull:
      xylo_unreachable();
      break;

    case TypeRepr::Kind::kNamed:
      VisitNamedTypeRepr(type_repr->As<NamedTypeRepr>(), ctx);
      break;

    case TypeRepr::Kind::kFunction:
      VisitFunctionTypeRepr(type_repr->As<FunctionTypeRepr>(), ctx);
      break;

    case TypeRepr::Kind::kTuple:
      VisitTupleTypeRepr(type_repr->As<TupleTypeRepr>(), ctx);
      break;
  }
}


void Resolver::VisitNamedTypeRepr(NamedTypeRepr* type_repr, ResolutionContext* ctx) {
  auto symbol = ctx->name_table->Lookup(type_repr->name());

  if (symbol == nullptr) {
    ErrorUndeclared(type_repr->position(), type_repr->name());
  } else if (symbol->kind() != Symbol::Kind::kClass) {
    ErrorValueAsType(type_repr->position(), symbol);
  } else {
    type_repr->set_symbol(symbol);
  }
}


void Resolver::VisitFunctionTypeRepr(FunctionTypeRepr* type_repr, ResolutionContext* ctx) {
  VisitTupleTypeRepr(type_repr->params_type(), ctx);
  VisitTypeRepr(type_repr->return_type(), ctx);
}


void Resolver::VisitTupleTypeRepr(TupleTypeRepr* type_repr, ResolutionContext* ctx) {
  for (auto& element : type_repr->elements()) {
    VisitTypeRepr(element.get(), ctx);
  }
}


void Resolver::CollectCapturedSymbols(FunctionExpression* func) {
  for (auto& param : func->params()) {
    auto symbol = param->symbol();
    MarkCapturedSymbol(func, symbol);
  }
}


void Resolver::CollectCapturedSymbols(FunctionExpression* func, Block* block) {
  for (auto& stmt : block->statements()) {
    switch (stmt->kind()) {
      case Statement::Kind::kLet: {
        auto let_stmt = stmt->As<LetStatement>();
        MarkCapturedSymbol(func, let_stmt->symbol());
        break;
      }

      case Statement::Kind::kVar: {
        auto var_stmt = stmt->As<VarStatement>();
        MarkCapturedSymbol(func, var_stmt->symbol());
        break;
      }

      default:
        break;
    }
  }
}


void Resolver::MarkCapturedSymbol(FunctionExpression* func, Symbol* symbol) {
  if (symbol->is_captured()) {
    auto index = func->captured_symbols().size();
    symbol->set_captured_index(index);
    func->add_captured_symbol(symbol);
  }
}


ClassDeclaration* Resolver::GetClass(Symbol* symbol) {
  xylo_contract(symbol->kind() == Symbol::Kind::kClass);
  auto it = class_map_.find(symbol);
  xylo_contract(it != class_map_.end());
  return it->second;
}


FunctionExpression* Resolver::GetFunction(Symbol* symbol) {
  xylo_contract(symbol->kind() == Symbol::Kind::kFunction);
  auto it = function_map_.find(symbol);
  xylo_contract(it != function_map_.end());
  return it->second;
}


Resolver::EntityState& Resolver::GetClassState(ClassDeclaration* clazz) {
  auto it = class_states_.find(clazz);
  xylo_contract(it != class_states_.end());
  return it->second;
}


Resolver::EntityState& Resolver::GetFunctionState(FunctionExpression* func) {
  auto it = function_states_.find(func);
  xylo_contract(it != function_states_.end());
  return it->second;
}


void Resolver::RegisterDecledClass(Symbol* symbol, ClassDeclaration* clazz) {
  class_map_.emplace(symbol, clazz);
  class_states_.emplace(clazz, EntityState(clazz->inner_scope()));
}


void Resolver::RegisterDecledFunction(Symbol* symbol, FunctionExpression* func) {
  function_map_.emplace(symbol, func);
  function_states_.emplace(func, EntityState(func->inner_scope()));
}


void Resolver::RegisterLambdaFunction(FunctionExpression* func) {
  function_states_.emplace(func, EntityState(func->inner_scope()));
}


void Resolver::OnUpdateReferenceScope(ClassDeclaration* clazz, std::function<void(const EntityState&)>&& callback) {
  auto& state = GetClassState(clazz);
  callback(state);
  state.on_update_reference_scope.push_back(std::move(callback));
}


void Resolver::OnUpdateReferenceScope(FunctionExpression* func, std::function<void(const EntityState&)>&& callback) {
  auto& state = GetFunctionState(func);
  callback(state);
  state.on_update_reference_scope.push_back(std::move(callback));
}


void Resolver::InvokeUpdateReferenceScope(const EntityState& state) {
  for (auto& callback : state.on_update_reference_scope) {
    callback(state);
  }
}


void Resolver::MarkAsClosureIfNeeded(FunctionExpression* func, Symbol* reference_symbol) {
  auto reference_scope = reference_symbol->scope();
  MarkAsClosureIfNeeded(func, reference_scope);

  if (reference_scope->is_outer_than(func->inner_scope())) {
    reference_symbol->set_captured(true);
  }
}


void Resolver::MarkAsClosureIfNeeded(FunctionExpression* func, Scope* reference_scope) {
  auto& state = GetFunctionState(func);
  if (reference_scope->is_outer_than(state.reference_scope)) {
    state.reference_scope = reference_scope;

    if (reference_scope->is_outer_than(func->inner_scope())) {
      func->set_closure(true);
    }

    InvokeUpdateReferenceScope(state);
  }
}


void Resolver::MarkAsClosureIfNeeded(ClassDeclaration* clazz, Scope* reference_scope) {
  auto& state = GetClassState(clazz);
  if (reference_scope->is_outer_than(state.reference_scope)) {
    state.reference_scope = reference_scope;

    if (reference_scope->is_outer_than(clazz->inner_scope())) {
      clazz->set_closure(true);
    }

    InvokeUpdateReferenceScope(state);
  }
}


void Resolver::ErrorUndeclared(const SourceRange& position, Identifier* name) {
  ReportError(position, "undeclared identifier '" + name->str().cpp_str() + "'");
}


void Resolver::ErrorRedefinition(const SourceRange& position, Identifier* name, const Symbol* previous_symbol) {
  ReportError(position, "redefinition of '" + name->str().cpp_str() + "'");
  if (previous_symbol->position().start.line != 0) {
    ReportNote(previous_symbol->position(), "previous declaration is here");
  }
}


void Resolver::ErrorTypeAsValue(const SourceRange& position, Symbol* symbol) {
  ReportError(position, "type '" + symbol->name()->str().cpp_str() + "' cannot be used as a value");
}


void Resolver::ErrorValueAsType(const SourceRange& position, Symbol* symbol) {
  ReportError(position, "value '" + symbol->name()->str().cpp_str() + "' cannot be used as a type");
}


}  // namespace xylo
