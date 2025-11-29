
#include "xylo/codegen/function_lowerer.h"

#include "xylo/codegen/type_converter.h"
#include "xylo/util/vector.h"

namespace xylo {


llvm::Function* FunctionLowerer::GetOrBuild() {
  if (llvm_func_ != nullptr) {
    return llvm_func_;
  }

  llvm_func_ = BuildPrototype();
  BuildBody(llvm_func_);
  return llvm_func_;
}


llvm::Function* FunctionLowerer::BuildPrototype() {
  auto func_type = llvm::dyn_cast<llvm::FunctionType>(ZonkAndConvert(xylo_func()->type(), false));
  xylo_contract(func_type != nullptr);
  return llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, func_name(), llvm_module());
}


void FunctionLowerer::BuildBody(llvm::Function* function) {
  // entry block for mem2reg
  auto entry_bb = llvm::BasicBlock::Create(llvm_context(), "entry", function);

  auto body_bb = llvm::BasicBlock::Create(llvm_context(), "body", function);
  builder_.SetInsertPoint(body_bb);

  // closure environment parameter
  auto env_value = function->getArg(0);
  env_value->setName(parent()->kind() == CodegenScope::Kind::kClass ? "this" : "__env");
  set_closure_env_ptr(env_value);

  // build heap frame
  if (xylo_func()->has_closure()) {
    BuildHeapFrame();
    StoreToHeapFrame(0, env_value);
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


void FunctionLowerer::BuildHeapFrame() {
  // Heap frame structure
  Vector<llvm::Type*> frame_members;
  frame_members.push_back(llvm::PointerType::getUnqual(llvm_context()));  // parent frame pointer
  for (auto symbol : xylo_func()->captured_symbols()) {
    auto type = ZonkAndConvert(symbol->type(), true);
    frame_members.push_back(type);
  }
  auto type = llvm::StructType::create(llvm_context(), TypeConverter::ToArrayRef(frame_members), "HeapFrame");
  set_heap_frame_type(type);

  // allocate heap frame
  auto ptr = BuildHeapAlloc(heap_frame_type());
  set_heap_frame_ptr(ptr);
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
      return BuildFunctionExpression(expr->As<FunctionExpression>());

    case Expression::Kind::kApply:
      return BuildApplyExpression(expr->As<ApplyExpression>());

    case Expression::Kind::kUnary:
      return BuildUnaryExpression(expr->As<UnaryExpression>());

    case Expression::Kind::kBinary:
      return BuildBinaryExpression(expr->As<BinaryExpression>());

    case Expression::Kind::kConditional:
      return BuildConditionalExpression(expr->As<ConditionalExpression>());

    case Expression::Kind::kNew:
      return BuildNewExpression(expr->As<NewExpression>());

    case Expression::Kind::kProjection:
      return BuildProjectionExpression(expr->As<ProjectionExpression>(), nullptr);

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
    case Symbol::Kind::kClass:
      xylo_unreachable();
      break;

    case Symbol::Kind::kLetVariable:
    case Symbol::Kind::kVarVariable:
      if (symbol->is_captured() && symbol->scope()->depth() < xylo_func()->scope()->depth()) {
        return BuildClosureEnvIdentifier(expr, out_closure_env);
      } else {
        return BuildLocalValueIdentifier(expr, out_closure_env);
      }

    case Symbol::Kind::kFunction:
      return BuildFunctionIdentifier(expr, out_closure_env);
  }

  xylo_unreachable();
}


llvm::Value* FunctionLowerer::BuildClosureEnvIdentifier(IdentifierExpression* expr, llvm::Value** out_closure_env) {
  auto symbol = expr->symbol();
  xylo_contract(symbol->is_captured());

  switch (symbol->kind()) {
    case Symbol::Kind::kLetVariable: {
      auto [ptr, type] = LoadClosureEnvironmentValuePtr(symbol);
      return builder_.CreateLoad(type, ptr);
    }

    case Symbol::Kind::kVarVariable: {
      auto [ptr, type] = LoadClosureEnvironmentValuePtr(symbol);

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


llvm::Value* FunctionLowerer::BuildLocalValueIdentifier(IdentifierExpression* expr, llvm::Value** out_closure_env) {
  auto symbol = expr->symbol();

  switch (symbol->kind()) {
    case Symbol::Kind::kLetVariable: {
      auto it = variable_value_map_.find(symbol);
      xylo_contract(it != variable_value_map_.end());
      return it->second;
    }

    case Symbol::Kind::kVarVariable: {
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

  Vector<Type*> type_args;
  TypeSink allocated;

  for (auto var : expr->instantiated_vars()) {
    type_args.push_back(var->Zonk(type_env(), false, &allocated));
  }
  auto llvm_func = GetOrBuildFunction(symbol, type_args);
  auto xylo_func = GetXyloFunction(symbol);

  if (xylo_func->is_closure()) {
    auto [env_ptr, _] = LoadClosureEnvironmentPtr(symbol);

    if (out_closure_env != nullptr) {
      // return func and env
      *out_closure_env = env_ptr;
      return llvm_func;
    } else {
      // return packed closure
      return BuildClosurePointer(llvm_func, env_ptr);
    }

  } else {
    // return regular function
    if (out_closure_env != nullptr) {
      *out_closure_env = null_ptr();
    }
    return llvm_func;
  }
}


llvm::Value* FunctionLowerer::BuildFunctionExpression(FunctionExpression* expr) {
  auto ext_env = std::make_unique<Substitution>(type_env());
  auto nested_lowerer = new FunctionLowerer(this, std::move(ext_env), expr);
  nested_lowerer->set_func_name("__anon");
  auto llvm_func = nested_lowerer->GetOrBuild();

  if (expr->is_closure()) {
    return BuildClosurePointer(llvm_func, heap_frame_ptr());
  } else {
    return llvm_func;
  }
}


llvm::Value* FunctionLowerer::BuildApplyExpression(ApplyExpression* expr) {
  llvm::Value* closure_env_ptr = null_ptr();

  TypeSink allocated;
  auto xylo_func_type = expr->func()->type()->Zonk(type_env(), false, &allocated)->As<FunctionType>();

  // prepare argument values
  Vector<llvm::Value*> arg_values;
  arg_values.push_back(closure_env_ptr);
  for (size_t i = 0; i < expr->args()->elements().size(); ++i) {
    auto arg_expr = expr->args()->elements()[i].get();
    auto arg_type = arg_expr->type()->Zonk(type_env(), false, &allocated);
    auto param_type = xylo_func_type->params_type()->elements()[i];

    auto arg_value = BuildExpression(arg_expr);
    arg_value = AdjustType(arg_value, arg_type, param_type);
    arg_values.push_back(arg_value);
  }

  // build function value
  llvm::Value* func_value;
  switch (expr->func()->kind()) {
    case Expression::Kind::kIdentifier:
      func_value = BuildIdentifierExpression(expr->func()->As<IdentifierExpression>(), &closure_env_ptr);
      arg_values[0] = closure_env_ptr;
      break;

    case Expression::Kind::kProjection:
      func_value = BuildProjectionExpression(expr->func()->As<ProjectionExpression>(), &closure_env_ptr);
      arg_values[0] = closure_env_ptr;
      break;

    default:
      func_value = BuildExpression(expr->func());
      break;
  }

  auto func = llvm::dyn_cast<llvm::Function>(func_value);
  if (func != nullptr) {
    // direct function call
    return builder_.CreateCall(func, TypeConverter::ToArrayRef(arg_values));

  } else {
    // function pointer or closure pointer call. check function type
    TypeConverter type_converter(xylo_context(), llvm_context());
    auto llvm_func_type = llvm::dyn_cast<llvm::FunctionType>(type_converter.Convert(xylo_func_type, false));
    xylo_contract(llvm_func_type != nullptr);

    if (xylo_func_type->is_closure()) {
      // extract function and environment from closure
      auto closure_ptr = func_value;
      auto ptr_type = llvm::PointerType::getUnqual(llvm_context());

      auto func_ptr_field = builder_.CreateStructGEP(closure_type(), closure_ptr, 0);
      func_value = builder_.CreateLoad(ptr_type, func_ptr_field);

      auto env_ptr_field = builder_.CreateStructGEP(closure_type(), closure_ptr, 1);
      auto env_ptr = builder_.CreateLoad(ptr_type, env_ptr_field);
      arg_values[0] = env_ptr;
    }

    return builder_.CreateCall(llvm_func_type, func_value, TypeConverter::ToArrayRef(arg_values));
  }
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

    case Token::kAssign:
      return builder_.CreateStore(ZonkAndAdjustType(rhs_value, rhs_xylo_type, lhs_xylo_type), lhs_value);

    default:
      xylo_unreachable();
  }
}


llvm::Value* FunctionLowerer::BuildConditionalExpression(ConditionalExpression* expr) {
  TypeSink allocated;
  auto then_expr_type = expr->then_expr()->type();
  auto else_expr_type = expr->else_expr()->type();
  auto merged_type = expr->type()->Zonk(type_env(), false, &allocated);
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
      TypeSink allocated;
      auto zonked_branch_type = type->Zonk(type_env(), false, &allocated);
      value = AdjustType(value, zonked_branch_type, merged_type);
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


llvm::Value* FunctionLowerer::BuildNewExpression(NewExpression* expr) {
  auto nominal_type = expr->type()->As<NominalType>();
  auto struct_type = GetOrCreateStruct(expr->class_symbol());
  auto ptr = BuildHeapAlloc(struct_type);

  BuildObjectInitializer(expr->initializer(), nominal_type, ptr);

  return ptr;
}


void FunctionLowerer::BuildExpressionInitializer(ExpressionInitializer* init, xylo::Type* var_type, llvm::Value* ptr) {
  auto init_value = BuildExpression(init->expr());
  auto store_value = ZonkAndAdjustType(init_value, init->expr()->type(), var_type);
  builder_.CreateStore(store_value, ptr);
}


void FunctionLowerer::BuildObjectInitializer(ObjectInitializer* init, xylo::NominalType* obj_type, llvm::Value* ptr) {
  auto class_lowerer = root()->GetClassLowerer(obj_type);
  xylo_contract(class_lowerer != nullptr);

  auto class_symbol = class_lowerer->class_symbol();
  auto struct_type = class_lowerer->GetOrCreate();

  // store closure environment pointer
  auto [env_ptr, _] = LoadClosureEnvironmentPtr(class_symbol);
  auto env_ptr_field = builder_.CreateStructGEP(struct_type, ptr, 0);
  builder_.CreateStore(env_ptr, env_ptr_field);

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


llvm::Value* FunctionLowerer::BuildProjectionExpression(ProjectionExpression* expr, llvm::Value** out_closure_env) {
  TypeSink allocated;
  auto object_ptr = BuildExpression(expr->object());
  auto object_type = expr->object()->type()->Zonk(type_env(), false, &allocated)->As<NominalType>();
  auto struct_type = GetStructType(object_type);

  Vector<MemberInfo*> member_path;
  object_type->GetMemberPath(expr->member_name(), &member_path);
  xylo_contract(!member_path.empty());

  auto member_info = member_path[0];
  xylo_contract(member_info != nullptr);

  auto get_field_ptr = [&](int limit) {
    Vector<llvm::Value*> indices;
    indices.push_back(builder_.getInt64(0));

    // reverse path to build GEP
    for (int i = member_path.size() - 1; i >= limit; --i) {
      auto member = member_path[i];
      indices.push_back(builder_.getInt32(member->index() + 1));
    }

    return builder_.CreateInBoundsGEP(struct_type, object_ptr, TypeConverter::ToArrayRef(indices));
  };

  auto get_method_owner_ptr = [&]() {
    if (member_path.size() == 1) {
      return object_ptr;
    } else {
      return get_field_ptr(1);
    }
  };

  switch (member_info->kind()) {
    case MemberInfo::Kind::kField: {
      auto field_ptr = get_field_ptr(0);

      if (expr->is_lvalue()) {
        return field_ptr;
      } else {
        auto field_type = ZonkAndConvert(expr->type(), true);
        return builder_.CreateLoad(field_type, field_ptr);
      }
    }

    case MemberInfo::Kind::kEmbedding:
      return get_field_ptr(0);

    case MemberInfo::Kind::kMethod: {
      Vector<Type*> type_args;
      for (auto var : expr->member_req()->instantiated_vars()) {
        type_args.push_back(var->Zonk(type_env(), false, &allocated));
      }
      auto llvm_func = GetOrBuildMethod(member_info->owner(), expr->member_name(), type_args);
      object_ptr = get_method_owner_ptr();

      if (out_closure_env != nullptr) {
        // return func and object pointer as env
        *out_closure_env = object_ptr;
        return llvm_func;
      } else {
        // return packed closure
        return BuildClosurePointer(llvm_func, object_ptr);
      }
    }
  }

  xylo_unreachable();
}


llvm::Value* FunctionLowerer::BuildBlockExpression(BlockExpression* expr) {
  return BuildBlock(expr->block());
}


llvm::Type* FunctionLowerer::ZonkAndConvert(xylo::Type* type, bool function_as_pointer) {
  TypeConverter type_converter(xylo_context(), llvm_context());
  TypeSink allocated;
  auto zonked_type = type->Zonk(type_env(), false, &allocated);
  auto llvm_type = type_converter.Convert(zonked_type, function_as_pointer);

  return llvm_type;
}


llvm::Value* FunctionLowerer::ZonkAndAdjustType(llvm::Value* value, xylo::Type* from_type, xylo::Type* to_type) {
  TypeSink allocated;
  auto zonked_from_type = from_type->Zonk(type_env(), false, &allocated);
  auto zonked_to_type = to_type->Zonk(type_env(), false, &allocated);

  return AdjustType(value, zonked_from_type, zonked_to_type);
}


llvm::Value* FunctionLowerer::AdjustType(llvm::Value* value, xylo::Type* zonked_from_type, xylo::Type* zonked_to_type) {
  if (zonked_from_type == zonked_to_type) {
    return value;
  }

  if (zonked_from_type == xylo_context()->int_type() && zonked_to_type == xylo_context()->float_type()) {
    return builder_.CreateSIToFP(value, llvm::Type::getDoubleTy(llvm_context()));
  }

  if (zonked_from_type->kind() == Type::Kind::kFunction && zonked_to_type->kind() == Type::Kind::kFunction) {
    auto from_func_type = zonked_from_type->As<FunctionType>();
    auto to_func_type = zonked_to_type->As<FunctionType>();

    if (from_func_type->params_type()->equals(to_func_type->params_type()) &&
        from_func_type->return_type()->equals(to_func_type->return_type())) {
      if (from_func_type->is_closure() == to_func_type->is_closure()) {
        // same function type
        return value;
      } else {
        // by type checking, regular function to closure function conversion
        xylo_contract(!from_func_type->is_closure());
        xylo_contract(to_func_type->is_closure());

        return BuildClosurePointer(value, null_ptr());
      }
    }

    if (!zonked_to_type->IsGroundType()) {
      // zonked_to_type contains an incomplete type like bottom, meaning this value will never be called
      return value;
    }
  }

#if 0  // enable later if needed
  if (zonked_from_type->is_bottom_type()) {
    TypeConverter converter(xylo_context(), llvm_context());
    auto llvm_to_type = converter.Convert(zonked_to_type);
    return llvm::UndefValue::get(llvm_to_type);
  }
#endif

  if (zonked_to_type->is_top_type()) {
    return value;
  }

  throw std::logic_error("unsupported type adjustment in FunctionLowerer::AdjustType");
}


llvm::Value* FunctionLowerer::BuildHeapAlloc(llvm::Type* type) {
  auto alloc_size = llvm_module()->getDataLayout().getTypeAllocSize(type);
  auto alloc_size_int = llvm::ConstantInt::get(size_type(), alloc_size);

  auto malloc_func = xylo_malloc();
  auto ptr = builder_.CreateCall(malloc_func, {alloc_size_int});

  return ptr;
}


llvm::Value* FunctionLowerer::StoreCapturedValue(Symbol* symbol, llvm::Value* value) {
  xylo_contract(symbol->is_captured());
  xylo_contract(symbol->scope()->depth() == xylo_func()->scope()->depth());

  auto index = symbol->captured_index() + 1;  // first index is for parent frame pointer
  return StoreToHeapFrame(index, value);
}


llvm::Value* FunctionLowerer::StoreToHeapFrame(int index, llvm::Value* value) {
  auto ptr = builder_.CreateStructGEP(heap_frame_type(), heap_frame_ptr(), index);
  builder_.CreateStore(value, ptr);
  return ptr;
}


llvm::Value* FunctionLowerer::LoadThisPointer() {
  auto lowerer = this->parent();
  auto env_ptr = this->closure_env_ptr();
  auto env_type = lowerer->scope_data_type();

  while (lowerer->kind() != CodegenScope::Kind::kClass) {
    auto parent_ptr_ptr = builder_.CreateStructGEP(env_type, env_ptr, 0);
    auto parent_ptr = builder_.CreateLoad(env_type->getElementType(0), parent_ptr_ptr);

    lowerer = lowerer->parent();
    env_ptr = parent_ptr;
    env_type = lowerer->scope_data_type();
  }

  return env_ptr;
}


std::pair<llvm::Value*, llvm::StructType*> FunctionLowerer::LoadClosureEnvironmentPtr(Symbol* symbol) {
  CodegenScope* lowerer;
  llvm::Value* env_ptr;
  llvm::StructType* env_type;

  if (symbol->scope()->depth() < this->scope_depth()) {
    lowerer = this->parent();
    env_ptr = this->closure_env_ptr();
    env_type = lowerer->scope_data_type();
  } else {
    lowerer = this;
    env_ptr = heap_frame_ptr();
    env_type = heap_frame_type();
  }

  while (symbol->scope()->depth() < lowerer->scope_depth()) {
    auto parent_ptr_ptr = builder_.CreateStructGEP(env_type, env_ptr, 0);
    auto parent_ptr = builder_.CreateLoad(env_type->getElementType(0), parent_ptr_ptr);

    lowerer = lowerer->parent();
    env_ptr = parent_ptr;
    env_type = lowerer->scope_data_type();
  }

  xylo_contract(symbol->scope()->depth() == lowerer->scope_depth());
  return {env_ptr, env_type};
}


std::pair<llvm::Value*, llvm::Type*> FunctionLowerer::LoadClosureEnvironmentValuePtr(Symbol* symbol) {
  auto [env_ptr, env_type] = LoadClosureEnvironmentPtr(symbol);

  auto index = symbol->captured_index() + 1;  // first index is for parent frame pointer
  auto value_ptr = builder_.CreateStructGEP(env_type, env_ptr, index);
  return {value_ptr, env_type->getElementType(index)};
}


llvm::Value* FunctionLowerer::BuildClosurePointer(llvm::Value* function, llvm::Value* env_ptr) {
  auto closure_ty = closure_type();
  auto closure_ptr = BuildHeapAlloc(closure_ty);

  auto func_ptr_field = builder_.CreateStructGEP(closure_ty, closure_ptr, 0);
  builder_.CreateStore(function, func_ptr_field);

  auto env_ptr_field = builder_.CreateStructGEP(closure_ty, closure_ptr, 1);
  builder_.CreateStore(env_ptr, env_ptr_field);

  return closure_ptr;
}


}  // namespace xylo
