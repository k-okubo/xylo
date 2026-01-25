
#include "xylo/codegen/function_lowerer.h"

#include "xylo/codegen/class_lowerer.h"
#include "xylo/codegen/type_converter.h"

namespace xylo {


llvm::Function* FunctionLowerer::GetOrBuildFunction() {
  if (llvm_func_ != nullptr) {
    return llvm_func_;
  }

  llvm_func_ = CreatePrototype();

  if (!never_called_) {
    BuildBody(llvm_func_);
  }

  return llvm_func_;
}


llvm::Function* FunctionLowerer::CreatePrototype() {
  TypeConverter tc(xylo_context(), llvm_context());
  TypeArena arena;
  auto zonked_type = xylo_func()->type()->Zonk(subst(), false, &arena);
  auto func_type = llvm::dyn_cast<llvm::FunctionType>(tc.Convert(zonked_type, false));
  xylo_contract(func_type != nullptr);

  auto name = llvm::StringRef(func_name().data(), func_name().size());
  auto prototype = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, name, llvm_module());

  // this function is not called if any parameter type remains unresolved
  for (auto param : zonked_type->As<xylo::FunctionType>()->params_type()->elements()) {
    if (param->is_bottom_type()) {
      never_called_ = true;
      break;
    }
  }

  if (never_called_) {
    auto entry_bb = llvm::BasicBlock::Create(llvm_context(), "entry", prototype);
    builder_.SetInsertPoint(entry_bb);
    builder_.CreateUnreachable();
  }

  return prototype;
}


void FunctionLowerer::BuildBody(llvm::Function* function) {
  // entry block for mem2reg
  auto entry_bb = llvm::BasicBlock::Create(llvm_context(), "entry", function);

  auto body_bb = llvm::BasicBlock::Create(llvm_context(), "body", function);
  builder_.SetInsertPoint(body_bb);

  // outer environment parameter
  auto env_value = function->getArg(0);
  env_value->setName(parent()->kind() == LoweringNode::Kind::kClass ? "this" : "__env");
  set_outer_env_ptr(env_value);

  // build inner environment
  if (xylo_func()->has_closure()) {
    BuildInnerEnv();
    StoreToInnerEnv(0, env_value);
  }

  // get other parameter values
  for (size_t i = 0; i < xylo_func()->params().size(); ++i) {
    auto param_symbol = xylo_func()->params()[i]->symbol();
    auto param_value = function->getArg(i + 1);
    param_value->setName(param_symbol->name()->str().cpp_view());
    variable_value_map_.emplace(param_symbol, param_value);

    if (param_symbol->is_captured()) {
      StoreCapturedValue(param_symbol, param_value);
    }
  }

  // build function body
  BuildBlock(xylo_func()->body());

  // finalize entry block
  builder_.SetInsertPoint(entry_bb);
  builder_.CreateBr(body_bb);
}


void FunctionLowerer::BuildInnerEnv() {
  // inner environment structure
  Vector<llvm::Type*> env_members;
  env_members.push_back(llvm::PointerType::getUnqual(llvm_context()));  // outer environment pointer
  for (auto symbol : xylo_func()->captured_symbols()) {
    auto type = ZonkAndConvert(symbol->type(), true);
    env_members.push_back(type);
  }
  auto type = llvm::StructType::create(llvm_context(), TypeConverter::ToArrayRef(env_members), "InnerEnv");
  set_inner_env_type(type);

  // allocate inner environment
  BuildingUtil bu(this, &builder_);
  auto ptr = bu.CreateHeapAlloc(inner_env_type());
  set_inner_env_ptr(ptr);
}


llvm::Value* FunctionLowerer::BuildBlock(Block* block) {
  for (auto& decl : block->declarations()) {
    RegisterDeclaration(decl.get());
  }

  llvm::Value* last_value = nullptr;
  for (auto& stmt : block->statements()) {
    last_value = BuildStatement(stmt.get());
  }

  return last_value;
}


llvm::Value* FunctionLowerer::BuildStatement(Statement* stmt) {
  switch (stmt->kind()) {
    case Statement::Kind::kExpression:
      return BuildExpressionStatement(stmt->As<ExpressionStatement>());

    case Statement::Kind::kLet:
      return BuildLetStatement(stmt->As<LetStatement>());

    case Statement::Kind::kVar:
      return BuildVarStatement(stmt->As<VarStatement>());

    case Statement::Kind::kReturn:
      return BuildReturnStatement(stmt->As<ReturnStatement>());
  }

  xylo_unreachable();
}


llvm::Value* FunctionLowerer::BuildExpressionStatement(ExpressionStatement* stmt) {
  return BuildExpression(stmt->expr());
}


llvm::Value* FunctionLowerer::BuildLetStatement(LetStatement* stmt) {
  auto symbol = stmt->symbol();
  auto value = BuildExpression(stmt->expr());
  variable_value_map_.emplace(symbol, value);

  if (symbol->is_captured()) {
    StoreCapturedValue(symbol, value);
  }

  return nullptr;
}


llvm::Value* FunctionLowerer::BuildVarStatement(VarStatement* stmt) {
  auto symbol = stmt->symbol();
  auto var_type = symbol->type();
  auto expr_type = stmt->expr()->type();
  auto value = BuildExpression(stmt->expr());
  value = ZonkAndAdjustType(value, expr_type, var_type);

  if (symbol->is_captured()) {
    auto ptr = StoreCapturedValue(symbol, value);
    variable_value_map_.emplace(symbol, ptr);

  } else {
    auto current_bb = builder_.GetInsertBlock();
    auto& entry_bb = current_bb->getParent()->getEntryBlock();

    // place alloca at entry block for mem2reg
    builder_.SetInsertPoint(&entry_bb);
    auto name = symbol->name()->str().cpp_view();
    auto ptr = builder_.CreateAlloca(value->getType(), nullptr, name);
    variable_value_map_.emplace(symbol, ptr);

    builder_.SetInsertPoint(current_bb);
    builder_.CreateStore(value, ptr);
  }

  return nullptr;
}


llvm::Value* FunctionLowerer::BuildReturnStatement(ReturnStatement* stmt) {
  builder_.CreateRet(BuildExpression(stmt->expr()));
  return nullptr;
}


llvm::Value* FunctionLowerer::BuildExpression(Expression* expr) {
  switch (expr->kind()) {
    case Expression::Kind::kNull:
      return BuildNullExpression(expr->As<NullExpression>());

    case Expression::Kind::kLiteral:
      return BuildLiteralExpression(expr->As<LiteralExpression>());

    case Expression::Kind::kThis:
      return BuildThisExpression(expr->As<ThisExpression>());

    case Expression::Kind::kIdentifier:
      return BuildIdentifierExpression(expr->As<IdentifierExpression>(), nullptr);

    case Expression::Kind::kTuple:
      xylo_unreachable();

    case Expression::Kind::kFunction:
      return BuildFunctionExpression(expr->As<FunctionExpression>(), nullptr);

    case Expression::Kind::kApply:
      return BuildApplyExpression(expr->As<ApplyExpression>());

    case Expression::Kind::kUnary:
      return BuildUnaryExpression(expr->As<UnaryExpression>());

    case Expression::Kind::kBinary:
      return BuildBinaryExpression(expr->As<BinaryExpression>());

    case Expression::Kind::kConditional:
      return BuildConditionalExpression(expr->As<ConditionalExpression>());

    case Expression::Kind::kConstruct:
      return BuildConstructExpression(expr->As<ConstructExpression>());

    case Expression::Kind::kSelect:
      return BuildSelectExpression(expr->As<SelectExpression>(), nullptr);

    case Expression::Kind::kBlock:
      return BuildBlockExpression(expr->As<BlockExpression>());
  }

  xylo_unreachable();
}


llvm::Value* FunctionLowerer::BuildNullExpression(NullExpression* expr) {
  auto type = ZonkAndConvert(expr->type(), true);
  return llvm::Constant::getNullValue(type);
}


llvm::Value* FunctionLowerer::BuildLiteralExpression(LiteralExpression* expr) {
  switch (expr->literal_kind()) {
    case LiteralExpression::LiteralKind::kBoolean: {
      auto bool_lit = expr->As<BooleanLiteral>();
      return builder_.getInt1(bool_lit->value());
    }

    case LiteralExpression::LiteralKind::kInteger: {
      auto int_lit = expr->As<IntegerLiteral>();
      return builder_.getInt64(int_lit->value());
    }

    case LiteralExpression::LiteralKind::kFloat: {
      auto float_lit = expr->As<FloatLiteral>();
      return llvm::ConstantFP::get(llvm::Type::getDoubleTy(llvm_context()), float_lit->value());
    }
  }

  xylo_unreachable();
}


llvm::Value* FunctionLowerer::BuildThisExpression(ThisExpression* expr) {
  return LoadThisPointer();
}


llvm::Value* FunctionLowerer::BuildIdentifierExpression(IdentifierExpression* expr, llvm::Value** out_closure_env) {
  auto symbol = expr->symbol();

  switch (symbol->kind()) {
    case Symbol::Kind::kType:
      xylo_unreachable();
      break;

    case Symbol::Kind::kLet:
    case Symbol::Kind::kVar:
      if (symbol->is_captured() && symbol->scope()->is_outer_than(xylo_func()->inner_scope())) {
        return BuildOuterEnvIdentifier(expr);
      } else {
        return BuildLocalValueIdentifier(expr);
      }

    case Symbol::Kind::kFunc:
      return BuildFunctionIdentifier(expr, out_closure_env);
  }

  xylo_unreachable();
}


llvm::Value* FunctionLowerer::BuildOuterEnvIdentifier(IdentifierExpression* expr) {
  auto symbol = expr->symbol();
  xylo_contract(symbol->is_captured());

  switch (symbol->kind()) {
    case Symbol::Kind::kLet: {
      auto [ptr, type] = LoadOuterEnvironmentValuePtr(symbol);
      return builder_.CreateLoad(type, ptr);
    }

    case Symbol::Kind::kVar: {
      auto [ptr, type] = LoadOuterEnvironmentValuePtr(symbol);

      if (expr->is_lvalue()) {
        return ptr;
      } else {
        return builder_.CreateLoad(type, ptr);
      }
    }

    default:
      xylo_unreachable();
      break;
  }
}


llvm::Value* FunctionLowerer::BuildLocalValueIdentifier(IdentifierExpression* expr) {
  auto symbol = expr->symbol();

  switch (symbol->kind()) {
    case Symbol::Kind::kLet: {
      auto it = variable_value_map_.find(symbol);
      xylo_contract(it != variable_value_map_.end());
      return it->second;
    }

    case Symbol::Kind::kVar: {
      auto it = variable_value_map_.find(symbol);
      xylo_contract(it != variable_value_map_.end());
      auto ptr = it->second;

      if (expr->is_lvalue()) {
        return ptr;
      } else {
        return builder_.CreateLoad(ZonkAndConvert(expr->type(), true), ptr);
      }
    }

    default:
      xylo_unreachable();
      break;
  }
}


llvm::Value* FunctionLowerer::BuildFunctionIdentifier(IdentifierExpression* expr, llvm::Value** out_closure_env) {
  auto symbol = expr->symbol();
  auto llvm_func = LoweringNode::GetOrBuildFunction(symbol, expr->type()->instantiated_info());
  auto xylo_func = GetXyloFunction(symbol);

  return ReturnFunction(llvm_func, out_closure_env, xylo_func->is_closure(), [this, symbol]() {
    auto [env_ptr, _] = LoadOuterEnvironmentPtr(symbol);
    return env_ptr;
  });
}


llvm::Value* FunctionLowerer::BuildFunctionExpression(FunctionExpression* expr, llvm::Value** out_closure_env) {
  auto ext_subst = std::make_unique<Substitution>(subst());
  auto nested_lowerer = new FunctionLowerer(this, std::move(ext_subst), expr);
  nested_lowerer->set_func_name(String("anon"));

  add_child(LoweringNodePtr(nested_lowerer));
  auto llvm_func = nested_lowerer->GetOrBuildFunction();

  return ReturnFunction(llvm_func, out_closure_env, expr->is_closure(), [this]() {
    return inner_env_ptr();
  });
}


llvm::Value* FunctionLowerer::BuildApplyExpression(ApplyExpression* expr) {
  BuildingUtil bu(this, &builder_);
  llvm::Value* closure_env_ptr = nullptr;

  TypeArena arena;
  auto xylo_func_type = expr->func()->type()->Zonk(subst(), false, &arena)->As<FunctionType>();

  // prepare argument values
  Vector<llvm::Value*> arg_values;
  arg_values.push_back(closure_env_ptr);
  for (size_t i = 0; i < expr->args()->elements().size(); ++i) {
    auto arg_expr = expr->args()->elements()[i].get();
    auto arg_type = arg_expr->type()->Zonk(subst(), false, &arena);
    auto param_type = xylo_func_type->params_type()->elements()[i];

    auto arg_value = BuildExpression(arg_expr);
    arg_value = bu.AdjustType(arg_value, arg_type, param_type);
    arg_values.push_back(arg_value);
  }

  // build function value
  llvm::Value* func_value;
  switch (expr->func()->kind()) {
    case Expression::Kind::kIdentifier:
      func_value = BuildIdentifierExpression(expr->func()->As<IdentifierExpression>(), &closure_env_ptr);
      arg_values[0] = closure_env_ptr;
      break;

    case Expression::Kind::kFunction:
      func_value = BuildFunctionExpression(expr->func()->As<FunctionExpression>(), &closure_env_ptr);
      arg_values[0] = closure_env_ptr;
      break;

    case Expression::Kind::kSelect:
      func_value = BuildSelectExpression(expr->func()->As<SelectExpression>(), &closure_env_ptr);
      arg_values[0] = closure_env_ptr;
      break;

    default:
      func_value = BuildExpression(expr->func());
      break;
  }

  // convert function type
  TypeConverter tc(xylo_context(), llvm_context());
  auto llvm_func_type = llvm::dyn_cast<llvm::FunctionType>(tc.Convert(xylo_func_type, false));
  xylo_contract(llvm_func_type != nullptr);

  if (closure_env_ptr != nullptr) {
    return builder_.CreateCall(llvm_func_type, func_value, tc.ToArrayRef(arg_values));
  }

  if (xylo_func_type->is_closure()) {
    // extract function and environment from closure
    auto closure_ptr = func_value;
    auto func_ptr = bu.LoadFunctionPtrFromClosureObject(closure_ptr);
    auto env_ptr = bu.LoadEnvironmentPtrFromClosureObject(closure_ptr);
    func_value = func_ptr;
    arg_values[0] = env_ptr;

  } else {
    arg_values[0] = null_ptr();
  }

  return builder_.CreateCall(llvm_func_type, func_value, tc.ToArrayRef(arg_values));
}


llvm::Value* FunctionLowerer::BuildUnaryExpression(UnaryExpression* expr) {
  auto operand_value = BuildExpression(expr->operand());

  switch (expr->op()) {
    case Token::kAdd:
      // do nothing
      return operand_value;

    case Token::kSub:
      if (operand_value->getType()->isIntegerTy()) {
        return builder_.CreateNeg(operand_value);
      } else {
        return builder_.CreateFNeg(operand_value);
      }

    case Token::kNot:
    case Token::kLNot:
      return builder_.CreateNot(operand_value);

    default:
      xylo_unreachable();
  }
}


llvm::Value* FunctionLowerer::BuildBinaryExpression(BinaryExpression* expr) {
  auto to_float = [this](llvm::Value* value) {
    if (value->getType()->isIntegerTy()) {
      return builder_.CreateSIToFP(value, llvm::Type::getDoubleTy(llvm_context()));
    } else {
      return value;
    }
  };

  auto lhs_value = BuildExpression(expr->lhs());
  auto rhs_value = BuildExpression(expr->rhs());

  auto lhs_xylo_type = expr->lhs()->type();
  auto rhs_xylo_type = expr->rhs()->type();

  switch (expr->op()) {
    case Token::kAdd:
      if (lhs_value->getType()->isIntegerTy() && rhs_value->getType()->isIntegerTy()) {
        return builder_.CreateAdd(lhs_value, rhs_value);
      } else {
        lhs_value = to_float(lhs_value);
        rhs_value = to_float(rhs_value);
        return builder_.CreateFAdd(lhs_value, rhs_value);
      }

    case Token::kSub:
      if (lhs_value->getType()->isIntegerTy() && rhs_value->getType()->isIntegerTy()) {
        return builder_.CreateSub(lhs_value, rhs_value);
      } else {
        lhs_value = to_float(lhs_value);
        rhs_value = to_float(rhs_value);
        return builder_.CreateFSub(lhs_value, rhs_value);
      }

    case Token::kMul:
      if (lhs_value->getType()->isIntegerTy() && rhs_value->getType()->isIntegerTy()) {
        return builder_.CreateMul(lhs_value, rhs_value);
      } else {
        lhs_value = to_float(lhs_value);
        rhs_value = to_float(rhs_value);
        return builder_.CreateFMul(lhs_value, rhs_value);
      }

    case Token::kDiv:
      if (lhs_value->getType()->isIntegerTy() && rhs_value->getType()->isIntegerTy()) {
        return builder_.CreateSDiv(lhs_value, rhs_value);
      } else {
        lhs_value = to_float(lhs_value);
        rhs_value = to_float(rhs_value);
        return builder_.CreateFDiv(lhs_value, rhs_value);
      }

    case Token::kRem:
      if (lhs_value->getType()->isIntegerTy() && rhs_value->getType()->isIntegerTy()) {
        return builder_.CreateSRem(lhs_value, rhs_value);
      } else {
        lhs_value = to_float(lhs_value);
        rhs_value = to_float(rhs_value);
        return builder_.CreateFRem(lhs_value, rhs_value);
      }

    case Token::kAnd:
    case Token::kLAnd: {
      return builder_.CreateAnd(lhs_value, rhs_value);
    }

    case Token::kOr:
    case Token::kLOr:
      return builder_.CreateOr(lhs_value, rhs_value);

    case Token::kXor:
      return builder_.CreateXor(lhs_value, rhs_value);

    case Token::kShl:
      return builder_.CreateShl(lhs_value, rhs_value);

    case Token::kShr:
      return builder_.CreateAShr(lhs_value, rhs_value);

    case Token::kEQ:
      if (lhs_value->getType()->isIntegerTy()) {
        return builder_.CreateICmpEQ(lhs_value, rhs_value);
      } else if (lhs_value->getType()->isFloatingPointTy()) {
        return builder_.CreateFCmpOEQ(lhs_value, rhs_value);
      } else if (lhs_value->getType()->isPointerTy()) {
        return builder_.CreateICmpEQ(lhs_value, rhs_value);
      } else {
        xylo_unreachable();
      }

    case Token::kNE:
      if (lhs_value->getType()->isIntegerTy()) {
        return builder_.CreateICmpNE(lhs_value, rhs_value);
      } else if (lhs_value->getType()->isFloatingPointTy()) {
        return builder_.CreateFCmpONE(lhs_value, rhs_value);
      } else if (lhs_value->getType()->isPointerTy()) {
        return builder_.CreateICmpNE(lhs_value, rhs_value);
      } else {
        xylo_unreachable();
      }

    case Token::kLT:
      if (lhs_value->getType()->isIntegerTy()) {
        return builder_.CreateICmpSLT(lhs_value, rhs_value);
      } else {
        return builder_.CreateFCmpOLT(lhs_value, rhs_value);
      }

    case Token::kLE:
      if (lhs_value->getType()->isIntegerTy()) {
        return builder_.CreateICmpSLE(lhs_value, rhs_value);
      } else {
        return builder_.CreateFCmpOLE(lhs_value, rhs_value);
      }

    case Token::kGT:
      if (lhs_value->getType()->isIntegerTy()) {
        return builder_.CreateICmpSGT(lhs_value, rhs_value);
      } else {
        return builder_.CreateFCmpOGT(lhs_value, rhs_value);
      }

    case Token::kGE:
      if (lhs_value->getType()->isIntegerTy()) {
        return builder_.CreateICmpSGE(lhs_value, rhs_value);
      } else {
        return builder_.CreateFCmpOGE(lhs_value, rhs_value);
      }

    case Token::kAssign: {
      rhs_value = ZonkAndAdjustType(rhs_value, rhs_xylo_type, lhs_xylo_type);
      return builder_.CreateStore(rhs_value, lhs_value);
    }

    case Token::kAddAssign: {
      llvm::Value* lhs_loaded = builder_.CreateLoad(ZonkAndConvert(lhs_xylo_type, true), lhs_value);
      llvm::Value* result;
      if (lhs_loaded->getType()->isIntegerTy()) {
        result = builder_.CreateAdd(lhs_loaded, rhs_value);
      } else {
        rhs_value = to_float(rhs_value);
        result = builder_.CreateFAdd(lhs_loaded, rhs_value);
      }
      builder_.CreateStore(result, lhs_value);
      return result;
    }

    case Token::kSubAssign: {
      llvm::Value* lhs_loaded = builder_.CreateLoad(ZonkAndConvert(lhs_xylo_type, true), lhs_value);
      llvm::Value* result;
      if (lhs_loaded->getType()->isIntegerTy()) {
        result = builder_.CreateSub(lhs_loaded, rhs_value);
      } else {
        rhs_value = to_float(rhs_value);
        result = builder_.CreateFSub(lhs_loaded, rhs_value);
      }
      builder_.CreateStore(result, lhs_value);
      return result;
    }

    case Token::kMulAssign: {
      llvm::Value* lhs_loaded = builder_.CreateLoad(ZonkAndConvert(lhs_xylo_type, true), lhs_value);
      llvm::Value* result;
      if (lhs_loaded->getType()->isIntegerTy()) {
        result = builder_.CreateMul(lhs_loaded, rhs_value);
      } else {
        rhs_value = to_float(rhs_value);
        result = builder_.CreateFMul(lhs_loaded, rhs_value);
      }
      builder_.CreateStore(result, lhs_value);
      return result;
    }

    case Token::kDivAssign: {
      llvm::Value* lhs_loaded = builder_.CreateLoad(ZonkAndConvert(lhs_xylo_type, true), lhs_value);
      llvm::Value* result;
      if (lhs_loaded->getType()->isIntegerTy()) {
        result = builder_.CreateSDiv(lhs_loaded, rhs_value);
      } else {
        rhs_value = to_float(rhs_value);
        result = builder_.CreateFDiv(lhs_loaded, rhs_value);
      }
      builder_.CreateStore(result, lhs_value);
      return result;
    }

    case Token::kRemAssign: {
      llvm::Value* lhs_loaded = builder_.CreateLoad(ZonkAndConvert(lhs_xylo_type, true), lhs_value);
      llvm::Value* result;
      if (lhs_loaded->getType()->isIntegerTy()) {
        result = builder_.CreateSRem(lhs_loaded, rhs_value);
      } else {
        rhs_value = to_float(rhs_value);
        result = builder_.CreateFRem(lhs_loaded, rhs_value);
      }
      builder_.CreateStore(result, lhs_value);
      return result;
    }

    case Token::kAndAssign: {
      llvm::Value* lhs_loaded = builder_.CreateLoad(ZonkAndConvert(lhs_xylo_type, true), lhs_value);
      auto result = builder_.CreateAnd(lhs_loaded, rhs_value);
      builder_.CreateStore(result, lhs_value);
      return result;
    }

    case Token::kOrAssign: {
      llvm::Value* lhs_loaded = builder_.CreateLoad(ZonkAndConvert(lhs_xylo_type, true), lhs_value);
      auto result = builder_.CreateOr(lhs_loaded, rhs_value);
      builder_.CreateStore(result, lhs_value);
      return result;
    }

    case Token::kXorAssign: {
      llvm::Value* lhs_loaded = builder_.CreateLoad(ZonkAndConvert(lhs_xylo_type, true), lhs_value);
      auto result = builder_.CreateXor(lhs_loaded, rhs_value);
      builder_.CreateStore(result, lhs_value);
      return result;
    }

    case Token::kShlAssign: {
      llvm::Value* lhs_loaded = builder_.CreateLoad(ZonkAndConvert(lhs_xylo_type, true), lhs_value);
      auto result = builder_.CreateShl(lhs_loaded, rhs_value);
      builder_.CreateStore(result, lhs_value);
      return result;
    }

    case Token::kShrAssign: {
      llvm::Value* lhs_loaded = builder_.CreateLoad(ZonkAndConvert(lhs_xylo_type, true), lhs_value);
      auto result = builder_.CreateAShr(lhs_loaded, rhs_value);
      builder_.CreateStore(result, lhs_value);
      return result;
    }

    default:
      xylo_unreachable();
  }
}


llvm::Value* FunctionLowerer::BuildConditionalExpression(ConditionalExpression* expr) {
  TypeArena arena;
  auto then_expr_type = expr->then_expr()->type();
  auto else_expr_type = expr->else_expr()->type();
  auto merged_type = expr->type()->Zonk(subst(), false, &arena);
  bool merge_bb_reachable = expr->is_reachable_then() || expr->is_reachable_else();

  // create each block
  auto func = builder_.GetInsertBlock()->getParent();
  auto then_bb = llvm::BasicBlock::Create(llvm_context(), "then", func);
  auto else_bb = llvm::BasicBlock::Create(llvm_context(), "else", func);
  auto merge_bb = merge_bb_reachable ? llvm::BasicBlock::Create(llvm_context(), "ifcont", func) : nullptr;

  // condition expression
  auto cond_value = BuildExpression(expr->cond());
  builder_.CreateCondBr(cond_value, then_bb, else_bb);

  // branch processing lambda
  auto branch_proc = [=, this](Expression* expr, Type* type, bool reachable) -> llvm::Value* {
    auto value = BuildExpression(expr);
    if (!reachable) {
      return nullptr;
    }

    // adjust type if value is required in context
    if (merged_type != xylo_context()->unit_type()) {
      BuildingUtil bu(this, &builder_);
      TypeArena arena;
      auto zonked_branch_type = type->Zonk(subst(), false, &arena);
      value = bu.AdjustType(value, zonked_branch_type, merged_type);
    }

    builder_.CreateBr(merge_bb);
    return value;
  };

  // then branch
  builder_.SetInsertPoint(then_bb);
  auto then_value = branch_proc(expr->then_expr(), then_expr_type, expr->is_reachable_then());
  then_bb = builder_.GetInsertBlock();

  // else branch
  else_bb->moveAfter(then_bb);
  builder_.SetInsertPoint(else_bb);
  auto else_value = branch_proc(expr->else_expr(), else_expr_type, expr->is_reachable_else());
  else_bb = builder_.GetInsertBlock();

  if (!merge_bb_reachable) {
    return nullptr;  // unreachable
  }

  // merge block
  merge_bb->moveAfter(else_bb);
  builder_.SetInsertPoint(merge_bb);

  if (merged_type == xylo_context()->unit_type()) {
    return nullptr;  // reachable but value not required
  }

  auto phi = builder_.CreatePHI(then_value->getType(), 2);
  if (expr->is_reachable_then()) {
    phi->addIncoming(then_value, then_bb);
  }
  if (expr->is_reachable_else()) {
    phi->addIncoming(else_value, else_bb);
  }

  return phi;
}


llvm::Value* FunctionLowerer::BuildConstructExpression(ConstructExpression* expr) {
  auto nominal_type = expr->type()->As<NominalType>();
  auto struct_type = GetOrCreateInstanceStruct(expr->type()->As<NominalType>());

  BuildingUtil bu(this, &builder_);
  auto ptr = bu.CreateHeapAlloc(struct_type);

  BuildObjectInitializer(expr->initializer(), nominal_type, ptr);

  return ptr;
}


void FunctionLowerer::BuildExpressionInitializer(ExpressionInitializer* init, xylo::Type* var_type, llvm::Value* ptr) {
  auto init_value = BuildExpression(init->expr());
  auto store_value = ZonkAndAdjustType(init_value, init->expr()->type(), var_type);
  builder_.CreateStore(store_value, ptr);
}


void FunctionLowerer::BuildObjectInitializer(ObjectInitializer* init, xylo::NominalType* obj_type, llvm::Value* ptr) {
  auto class_lowerer = GetClassLowerer(obj_type);
  xylo_contract(class_lowerer != nullptr);

  auto class_decl = class_lowerer->class_decl();
  auto struct_type = class_lowerer->GetOrCreateInstanceStruct();

  // store outer environment pointer
  auto env_ptr_field = builder_.CreateStructGEP(struct_type, ptr, 0);
  if (class_decl->is_closure()) {
    auto [env_ptr, _] = LoadOuterEnvironmentPtr(class_decl->symbol());
    builder_.CreateStore(env_ptr, env_ptr_field);
  } else {
    builder_.CreateStore(null_ptr(), env_ptr_field);
  }

  // store field init values
  for (auto& entry : init->entries()) {
    auto field_name = entry->name();
    auto field_info = obj_type->GetMember(field_name);
    auto field_index = field_info->index();
    xylo_contract(field_index >= 0);
    auto field_ptr = builder_.CreateStructGEP(struct_type, ptr, field_index + 1);

    BuildFieldEntry(entry.get(), field_info->type(), field_ptr);
  }
}


void FunctionLowerer::BuildFieldEntry(FieldEntry* entry, xylo::Type* var_type, llvm::Value* ptr) {
  switch (entry->value()->kind()) {
    case Initializer::Kind::kExpression:
      BuildExpressionInitializer(entry->value()->As<ExpressionInitializer>(), var_type, ptr);
      break;

    case Initializer::Kind::kObject:
      BuildObjectInitializer(entry->value()->As<ObjectInitializer>(), var_type->As<NominalType>(), ptr);
      break;
  }
}


llvm::Value* FunctionLowerer::BuildSelectExpression(SelectExpression* expr, llvm::Value** out_closure_env) {
  TypeArena arena;
  auto object_type = expr->object()->type()->Zonk(subst(), false, &arena)->As<NominalType>();

  if (object_type->category() == NominalType::Category::kClass) {
    return BuildClassSelect(expr, object_type, out_closure_env);
  } else {
    xylo_contract(object_type->category() == NominalType::Category::kInterface);
    return BuildInterfaceSelect(expr, object_type, out_closure_env);
  }
}


llvm::Value* FunctionLowerer::BuildClassSelect(SelectExpression* expr, NominalType* object_type,
                                               llvm::Value** out_closure_env) {
  BuildingUtil bu(this, &builder_);

  auto object_ptr = BuildExpression(expr->object());
  auto instance_struct = GetInstanceStruct(object_type);

  Vector<NominalSlot*> member_path;
  object_type->GetMemberPath(expr->member_name(), &member_path);
  xylo_contract(!member_path.empty());
  auto member_info = member_path[0];

  switch (member_info->kind()) {
    case MemberInfo::Kind::kField: {
      auto field_ptr = bu.MemberPtr(instance_struct, object_ptr, member_path);

      if (expr->is_lvalue()) {
        return field_ptr;
      } else {
        auto field_type = ZonkAndConvert(expr->type(), true);
        return builder_.CreateLoad(field_type, field_ptr);
      }
    }

    case MemberInfo::Kind::kEmbedding:
      return bu.MemberPtr(instance_struct, object_ptr, member_path);

    case MemberInfo::Kind::kMethod: {
      TypeArena arena;
      auto instantiated_info = expr->type()->Zonk(subst(), false, &arena)->instantiated_info();
      auto llvm_func = GetOrBuildMethod(member_info->owner(), expr->member_name(), instantiated_info);

      // load method's owner pointer
      object_ptr = bu.MemberPtr(instance_struct, object_ptr, member_path, 1);  // skip method slot

      return ReturnFunction(llvm_func, out_closure_env, true, [object_ptr]() {
        return object_ptr;
      });
    }

    case MemberInfo::Kind::kSuper:
      xylo_unreachable();
  }

  xylo_unreachable();
}


llvm::Value* FunctionLowerer::BuildInterfaceSelect(SelectExpression* expr, NominalType* object_type,
                                                   llvm::Value** out_closure_env) {
  BuildingUtil bu(this, &builder_);

  auto interface_ptr = BuildExpression(expr->object());
  auto vtable_struct = GetVTableStruct(object_type);

  Vector<NominalSlot*> member_path;
  object_type->GetMemberPath(expr->member_name(), &member_path);
  xylo_contract(!member_path.empty());
  xylo_contract(member_path[0]->kind() == MemberInfo::Kind::kMethod);

  TypeConverter tc(xylo_context(), llvm_context());
  auto object_ptr = bu.LoadObjectPtrFromInterfaceFatptr(interface_ptr);
  auto vtable_ptr = bu.LoadVTablePtrFromInterfaceFatptr(interface_ptr);

  auto func_ptr_field = bu.MemberPtr(vtable_struct, vtable_ptr, member_path);
  auto func_ptr = builder_.CreateLoad(tc.PointerType(), func_ptr_field);

  return ReturnFunction(func_ptr, out_closure_env, true, [object_ptr]() {
    return object_ptr;
  });
}


llvm::Value* FunctionLowerer::BuildBlockExpression(BlockExpression* expr) {
  return BuildBlock(expr->block());
}


llvm::Value* FunctionLowerer::ZonkAndAdjustType(llvm::Value* value, xylo::Type* from_type, xylo::Type* to_type) {
  TypeArena arena;
  auto zonked_from_type = from_type->Zonk(subst(), false, &arena);
  auto zonked_to_type = to_type->Zonk(subst(), false, &arena);

  BuildingUtil bu(this, &builder_);
  return bu.AdjustType(value, zonked_from_type, zonked_to_type);
}


llvm::Value* FunctionLowerer::StoreCapturedValue(Symbol* symbol, llvm::Value* value) {
  xylo_contract(symbol->is_captured());
  xylo_contract(symbol->scope() == xylo_func()->inner_scope());

  auto index = symbol->captured_index() + 1;  // first index is for outer environment pointer
  return StoreToInnerEnv(index, value);
}


llvm::Value* FunctionLowerer::StoreToInnerEnv(int index, llvm::Value* value) {
  auto ptr = builder_.CreateStructGEP(inner_env_type(), inner_env_ptr(), index);
  builder_.CreateStore(value, ptr);
  return ptr;
}


llvm::Value* FunctionLowerer::LoadThisPointer() {
  auto lowerer = this->parent();
  auto env_ptr = this->outer_env_ptr();
  auto env_type = lowerer->scope_data_type();

  while (lowerer->kind() != LoweringNode::Kind::kClass) {
    auto parent_ptr_field = builder_.CreateStructGEP(env_type, env_ptr, 0);
    auto parent_ptr = builder_.CreateLoad(env_type->getElementType(0), parent_ptr_field);

    xylo_contract(lowerer->parent() != nullptr);
    lowerer = lowerer->parent();
    env_ptr = parent_ptr;
    env_type = lowerer->scope_data_type();
  }

  return env_ptr;
}


std::pair<llvm::Value*, llvm::StructType*> FunctionLowerer::LoadOuterEnvironmentPtr(Symbol* symbol) {
  LoweringNode* lowerer;
  llvm::Value* env_ptr;
  llvm::StructType* env_type;

  if (symbol->scope()->is_outer_than(this->scope())) {
    lowerer = this->parent();
    env_ptr = this->outer_env_ptr();
    env_type = lowerer->scope_data_type();
  } else {
    lowerer = this;
    env_ptr = this->inner_env_ptr();
    env_type = this->inner_env_type();
  }

  while (symbol->scope()->is_outer_than(lowerer->scope())) {
    auto parent_ptr_field = builder_.CreateStructGEP(env_type, env_ptr, 0);
    auto parent_ptr = builder_.CreateLoad(env_type->getElementType(0), parent_ptr_field);

    xylo_contract(lowerer->parent() != nullptr);
    lowerer = lowerer->parent();
    env_ptr = parent_ptr;
    env_type = lowerer->scope_data_type();
  }

  xylo_contract(symbol->scope() == lowerer->scope());
  return {env_ptr, env_type};
}


std::pair<llvm::Value*, llvm::Type*> FunctionLowerer::LoadOuterEnvironmentValuePtr(Symbol* symbol) {
  auto [env_ptr, env_type] = LoadOuterEnvironmentPtr(symbol);

  auto index = symbol->captured_index() + 1;  // first index is for outer environment pointer
  auto value_ptr = builder_.CreateStructGEP(env_type, env_ptr, index);
  return {value_ptr, env_type->getElementType(index)};
}


}  // namespace xylo
