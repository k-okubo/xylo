
#include "xylo/codegen/class_lowerer.h"

#include "xylo/codegen/type_converter.h"

namespace xylo {


llvm::StructType* ClassLowerer::GetOrCreateStruct() {
  if (llvm_struct_ != nullptr) {
    return llvm_struct_;
  }

  for (auto& member_decl : xylo_class()->declarations()) {
    RegisterDeclaration(member_decl.get());
    if (member_decl->kind() == Declaration::Kind::kFunction) {
      RegisterMethod(member_decl->symbol()->name(), member_decl->symbol());
    }
  }

  llvm_struct_ = CreateStruct();
  return llvm_struct_;
}


llvm::Function* ClassLowerer::GetOrBuildMethod(Identifier* name, const Vector<Type*>& type_args) {
  auto member_it = member_symbols_.find(name);
  xylo_contract(member_it != member_symbols_.end());

  auto method_symbol = member_it->second;
  return GetOrBuildFunction(method_symbol, type_args);
}


llvm::StructType* ClassLowerer::CreateStruct() {
  auto nominal = xylo_class()->symbol()->type()->As<NominalType>();
  TypeConverter type_converter(xylo_context(), llvm_context());
  return type_converter.CreateStructType(nominal);
}


void ClassLowerer::RegisterMethod(Identifier* name, Symbol* symbol) {
  member_symbols_.emplace(name, symbol);
}


}  // namespace xylo
