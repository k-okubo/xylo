
#include "xylo/codegen/interface_lowerer.h"

#include "xylo/codegen/type_converter.h"

namespace xylo {


llvm::StructType* InterfaceLowerer::GetOrCreateVTableStruct() {
  if (llvm_struct_ != nullptr) {
    return llvm_struct_;
  }

  llvm_struct_ = CreateVTableStruct();
  return llvm_struct_;
}


llvm::StructType* InterfaceLowerer::CreateVTableStruct() {
  for (auto& super : xylo_interface()->supers()) {
    CodegenScope::GetOrCreateVTableStruct(super->symbol());
  }

  TypeConverter tc(xylo_context(), llvm_context());
  auto nominal = xylo_interface()->symbol()->type()->As<NominalType>();

  Vector<llvm::Type*> vtable_entries;
  vtable_entries.push_back(tc.PointerType());  // dummy entry

  for (auto super : nominal->supers()) {
    vtable_entries.push_back(root()->GetInterfaceLowerer(super)->GetOrCreateVTableStruct());
  }

  for (auto method : nominal->methods()) {
    vtable_entries.push_back(tc.Convert(method->type(), true));
  }

  auto name = llvm::StringRef(interface_name().data(), interface_name().size());
  return llvm::StructType::create(llvm_context(), tc.ToArrayRef(vtable_entries), name);
}


}  // namespace xylo
