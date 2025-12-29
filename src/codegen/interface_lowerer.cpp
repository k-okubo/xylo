
#include "xylo/codegen/interface_lowerer.h"

#include "xylo/codegen/type_converter.h"

namespace xylo {


llvm::StructType* InterfaceLowerer::GetOrCreateVTableStruct() {
  if (vtable_struct_ != nullptr) {
    return vtable_struct_;
  }

  vtable_struct_ = CreateVTableStruct();
  return vtable_struct_;
}


llvm::StructType* InterfaceLowerer::CreateVTableStruct() {
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
