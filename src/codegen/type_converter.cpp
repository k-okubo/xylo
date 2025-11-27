
#include "xylo/codegen/type_converter.h"

#include <llvm/IR/DerivedTypes.h>

#include "xylo/syntax/substitution.h"
#include "xylo/util/vector.h"


llvm::Type* TypeConverter::Convert(xylo::Type* type, bool function_as_pointer) {
  switch (type->kind()) {
    case xylo::Type::Kind::kNominal:
      return ConvertNominalType(type->As<xylo::NominalType>());

    case xylo::Type::Kind::kFunction:
      return ConvertFunctionType(type->As<xylo::FunctionType>(), function_as_pointer);

    case xylo::Type::Kind::kIntersection:
      xylo_contract(type->is_top_type());
      return llvm::Type::getInt64Ty(llvm_context_);

    case xylo::Type::Kind::kUnion:
      xylo_contract(type->is_bottom_type());
      return llvm::Type::getInt64Ty(llvm_context_);

    case xylo::Type::Kind::kError:
    case xylo::Type::Kind::kMemberReq:
    case xylo::Type::Kind::kTuple:
    case xylo::Type::Kind::kTyvar:
    case xylo::Type::Kind::kMetavar:
    case xylo::Type::Kind::kScheme:
      xylo_unreachable();
  }

  xylo_unreachable();
}


llvm::Type* TypeConverter::ConvertNominalType(xylo::NominalType* type) {
  if (type == xylo_context_->unit_type()) {
    return llvm::Type::getInt8Ty(llvm_context_);
  }

  if (type == xylo_context_->bool_type()) {
    return llvm::Type::getInt1Ty(llvm_context_);
  }

  if (type == xylo_context_->int_type()) {
    return llvm::Type::getInt64Ty(llvm_context_);
  }

  if (type == xylo_context_->float_type()) {
    return llvm::Type::getDoubleTy(llvm_context_);
  }

  return llvm::PointerType::getUnqual(llvm_context_);
}


llvm::Type* TypeConverter::ConvertFunctionType(xylo::FunctionType* type, bool as_pointer) {
  if (as_pointer) {
    return llvm::PointerType::getUnqual(llvm_context_);
  }

  xylo::Vector<llvm::Type*> param_types;
  param_types.push_back(llvm::PointerType::getUnqual(llvm_context_));  // closure environment
  for (auto elem : type->params_type()->elements()) {
    param_types.push_back(Convert(elem, true));
  }

  auto return_type = Convert(type->return_type(), true);
  return llvm::FunctionType::get(return_type, ToArrayRef(param_types), false);
}


llvm::StructType* TypeConverter::CreateStructType(xylo::NominalType* type) {
  xylo::Vector<llvm::Type*> field_types;
  field_types.push_back(llvm::PointerType::getUnqual(llvm_context_));  // closure environment
  for (auto field : type->fields()) {
    xylo::Substitution env;
    xylo::TypeSink allocated;
    auto xylo_field_type = field->type()->Zonk(&env, false, &allocated);

    if (xylo_field_type != nullptr) {
      field_types.push_back(Convert(xylo_field_type, true));
    } else {
      field_types.push_back(llvm::Type::getInt64Ty(llvm_context_));
    }
  }

  auto name = type->name()->str().cpp_view();
  return llvm::StructType::create(llvm_context_, ToArrayRef(field_types), name);
}
