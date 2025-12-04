
#include "xylo/codegen/class_lowerer.h"

#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>

#include "xylo/codegen/interface_lowerer.h"
#include "xylo/codegen/pointer_adjuster.h"
#include "xylo/codegen/type_converter.h"

namespace xylo {


llvm::StructType* ClassLowerer::GetOrCreateInstanceStruct() {
  if (instance_struct_ != nullptr) {
    return instance_struct_;
  }

  for (auto& member_decl : xylo_class()->declarations()) {
    RegisterDeclaration(member_decl.get());
    if (member_decl->kind() == Declaration::Kind::kFunction) {
      RegisterMethod(member_decl->symbol()->name(), member_decl->symbol());
    }
  }

  // ensure embedded and super classes are created
  for (auto& embed : xylo_class()->embeddings()) {
    CodegenScope::GetOrCreateInstanceStruct(embed->symbol());
  }
  for (auto& super : xylo_class()->supers()) {
    CodegenScope::GetOrCreateVTableStruct(super->symbol());
  }

  // create struct
  instance_struct_ = CreateInstanceStruct();
  vtable_struct_ = GetOrCreateVTableStruct();

  // create vtable global
  vtable_global_ = CreateVTableGlobal();
  vtable_global_->setInitializer(CreateVTableConstant());

  return instance_struct_;
}


llvm::StructType* ClassLowerer::CreateInstanceStruct() {
  TypeConverter tc(xylo_context(), llvm_context());
  auto nominal = xylo_class()->symbol()->type()->As<NominalType>();

  auto entries_count = nominal->embeddings().size() + nominal->fields().size() + 1;
  Vector<llvm::Type*> struct_entries(entries_count);
  struct_entries[0] = tc.PointerType();  // closure environment

  for (auto embedding : nominal->embeddings()) {
    auto embed_type = embedding->type()->As<NominalType>();
    auto index = embedding->index();
    struct_entries[index + 1] = root()->GetClassLowerer(embed_type)->GetOrCreateInstanceStruct();
  }

  for (auto field : nominal->fields()) {
    auto index = field->index();
    struct_entries[index + 1] = tc.Convert(field->type(), true);
  }

  auto name = llvm::StringRef(class_name().data(), class_name().size());
  return llvm::StructType::create(llvm_context(), tc.ToArrayRef(struct_entries), name);
}


llvm::StructType* ClassLowerer::GetOrCreateVTableStruct() {
  TypeConverter tc(xylo_context(), llvm_context());

  Vector<llvm::Type*> vtable_entries;
  vtable_entries.push_back(tc.PointerType());  // dummy entry

  for (auto super : xylo_nominal()->supers()) {
    vtable_entries.push_back(root()->GetInterfaceLowerer(super)->GetOrCreateVTableStruct());
  }

  String vtable_name;
  vtable_name += class_name();
  vtable_name += "_vtable";

  auto name = llvm::StringRef(vtable_name.data(), vtable_name.size());
  return llvm::StructType::create(llvm_context(), tc.ToArrayRef(vtable_entries), name);
}


llvm::GlobalVariable* ClassLowerer::CreateVTableGlobal() {
  String constant_name;
  constant_name += class_name();
  constant_name += "_vtable_instance";

  auto linkage = llvm::GlobalValue::ExternalLinkage;
  auto name = llvm::StringRef(constant_name.data(), constant_name.size());
  return new llvm::GlobalVariable(*root()->llvm_module(), vtable_struct_, true, linkage, nullptr, name);
}


llvm::Constant* ClassLowerer::CreateVTableConstant() {
  TypeConverter tc(xylo_context(), llvm_context());

  Vector<llvm::Constant*> vtable_entries;
  vtable_entries.push_back(llvm::Constant::getNullValue(tc.PointerType()));  // dummy entry

  for (auto super_super : xylo_nominal()->supers()) {
    vtable_entries.push_back(CreateVTableConstant(super_super));
  }

  return llvm::ConstantStruct::get(vtable_struct_, tc.ToArrayRef(vtable_entries));
}


llvm::Constant* ClassLowerer::CreateVTableConstant(NominalType* super) {
  TypeConverter tc(xylo_context(), llvm_context());

  Vector<llvm::Constant*> vtable_entries;
  vtable_entries.push_back(llvm::Constant::getNullValue(tc.PointerType()));  // dummy entry

  for (auto super_super : super->supers()) {
    vtable_entries.push_back(CreateVTableConstant(super_super));
  }

  for (auto method : super->methods()) {
    vtable_entries.push_back(CreateVTableEntry(method));
  }

  auto vtable = root()->GetInterfaceLowerer(super)->GetOrCreateVTableStruct();
  return llvm::ConstantStruct::get(vtable, tc.ToArrayRef(vtable_entries));
}


llvm::Function* ClassLowerer::CreateVTableEntry(MemberInfo* super_method) {
  auto member_it = member_symbols_.find(super_method->name());
  if (member_it != member_symbols_.end()) {
    return GetOrBuildFunction(member_it->second, {});
  } else {
    return GetOrCreateEmbeddingMethodBridge(super_method);
  }
}


llvm::Function* ClassLowerer::GetOrCreateEmbeddingMethodBridge(MemberInfo* super_method) {
  // check cache
  auto bridge_it = bridge_methods_.find(super_method->name());
  if (bridge_it != bridge_methods_.end()) {
    return bridge_it->second;
  }

  TypeConverter tc(xylo_context(), llvm_context());
  auto method_type = super_method->type()->As<FunctionType>();

  // build bridge function type
  std::vector<llvm::Type*> param_types;
  param_types.push_back(tc.PointerType());  // this pointer
  for (auto& param_type : method_type->params_type()->elements()) {
    param_types.push_back(tc.Convert(param_type, true));
  }
  auto return_type = tc.Convert(method_type->return_type(), true);

  String bridge_name;
  bridge_name += class_name();
  bridge_name += "__";
  bridge_name += super_method->name()->str();

  // create bridge function
  auto name = llvm::StringRef(bridge_name.data(), bridge_name.size());
  auto bridge_type = llvm::FunctionType::get(return_type, param_types, false);
  auto bridge_func = llvm::Function::Create(bridge_type, llvm::Function::ExternalLinkage, name, llvm_module());

  // cache bridge function
  bridge_methods_.emplace(super_method->name(), bridge_func);

  // build bridge function body
  llvm::IRBuilder<> builder(llvm_context());
  auto bb = llvm::BasicBlock::Create(llvm_context(), "entry", bridge_func);
  builder.SetInsertPoint(bb);

  // get embedded object pointer
  Vector<NominalSlot*> member_path;
  xylo_nominal()->GetMemberPath(super_method->name(), &member_path);
  xylo_contract(!member_path.empty());

  auto member_info = member_path[0];
  xylo_contract(member_info->kind() == MemberInfo::Kind::kMethod);

  PointerAdjuster pa(&builder);
  auto this_ptr = bridge_func->getArg(0);
  auto obj_ptr = pa.StructFieldPtr(instance_struct_, this_ptr, member_path, 1);

  // get embedding method
  auto embedding_lowerer = root()->GetClassLowerer(member_info->owner());
  auto embedding_method = embedding_lowerer->GetOrBuildMethod(super_method->name(), {});

  // build call to embedding method
  Vector<llvm::Value*> arg_values;
  arg_values.push_back(obj_ptr);
  for (size_t i = 1; i < bridge_func->arg_size(); ++i) {
    arg_values.push_back(bridge_func->getArg(i));
  }

  auto return_value = builder.CreateCall(embedding_method, tc.ToArrayRef(arg_values));
  builder.CreateRet(return_value);

  return bridge_func;
}


llvm::Function* ClassLowerer::GetOrBuildMethod(Identifier* name, const Vector<Type*>& type_args) {
  auto member_it = member_symbols_.find(name);
  xylo_contract(member_it != member_symbols_.end());

  auto method_symbol = member_it->second;
  return GetOrBuildFunction(method_symbol, type_args);
}


void ClassLowerer::RegisterMethod(Identifier* name, Symbol* symbol) {
  member_symbols_.emplace(name, symbol);
}


}  // namespace xylo
