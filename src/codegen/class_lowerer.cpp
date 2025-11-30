
#include "xylo/codegen/class_lowerer.h"

#include "xylo/codegen/type_converter.h"

namespace xylo {


llvm::StructType* ClassLowerer::GetOrCreate() {
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
  // ensure embedded classes are created first
  for (auto& embed : xylo_class()->embeddings()) {
    GetOrCreateStruct(embed->symbol());
  }

  TypeConverter type_converter(xylo_context(), llvm_context());
  auto nominal = xylo_class()->symbol()->type()->As<NominalType>();

  Vector<llvm::Type*> field_types;
  field_types.push_back(llvm::PointerType::getUnqual(llvm_context()));  // closure environment

  for (auto field : nominal->fields()) {
    switch (field->kind()) {
      case MemberInfo::Kind::kField:
        field_types.push_back(type_converter.Convert(field->type(), true));
        break;

      case MemberInfo::Kind::kEmbedding: {
        auto embed_type = field->type()->As<NominalType>();
        auto embed_struct = root()->GetClassLowerer(embed_type)->GetOrCreate();
        field_types.push_back(embed_struct);
        break;
      }

      case MemberInfo::Kind::kMethod:
        xylo_unreachable();
    }
  }

  auto name = llvm::StringRef(class_name().data(), class_name().size());
  return llvm::StructType::create(llvm_context(), type_converter.ToArrayRef(field_types), name);
}


void ClassLowerer::RegisterMethod(Identifier* name, Symbol* symbol) {
  member_symbols_.emplace(name, symbol);
}


}  // namespace xylo
