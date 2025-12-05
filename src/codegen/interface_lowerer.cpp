
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
  // ensure vtable structs for superinterfaces are created
  for (auto& super : xylo_interface()->supers()) {
    LoweringNode::GetOrCreateVTableStruct(super->symbol());
  }

  TypeConverter tc(xylo_context(), llvm_context());
  auto nominal = xylo_nominal();

  Vector<llvm::Type*> vtable_entries;
  vtable_entries.push_back(tc.PointerType());  // dummy entry

  for (auto super : nominal->supers()) {
    vtable_entries.push_back(GetVTableStruct(super));
  }

  for (auto method : nominal->methods()) {
    vtable_entries.push_back(tc.Convert(method->type(), true));
  }

  return llvm::StructType::create(llvm_context(), tc.ToArrayRef(vtable_entries), tc.ToStringRef(interface_name_));
}


}  // namespace xylo
