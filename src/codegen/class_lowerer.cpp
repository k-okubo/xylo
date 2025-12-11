
#include "xylo/codegen/class_lowerer.h"

#include "xylo/codegen/building_util.h"
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
  for (auto& embedded : xylo_class()->embeddeds()) {
    LoweringNode::GetOrCreateInstanceStruct(embedded->symbol());
  }
  for (auto& super : xylo_class()->supers()) {
    LoweringNode::GetOrCreateVTableStruct(super->symbol());
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
  auto nominal = xylo_nominal();

  auto entries_count = nominal->embeddeds().size() + nominal->fields().size() + 1;
  Vector<llvm::Type*> struct_entries(entries_count);
  struct_entries[0] = tc.PointerType();  // outer environment

  for (auto embedded : nominal->embeddeds()) {
    auto embedded_type = embedded->type()->As<NominalType>();
    auto index = embedded->index();
    struct_entries[index + 1] = GetInstanceStruct(embedded_type);
  }

  for (auto field : nominal->fields()) {
    auto index = field->index();
    struct_entries[index + 1] = tc.Convert(field->type(), true);
  }

  return llvm::StructType::create(llvm_context(), tc.ToArrayRef(struct_entries), tc.ToStringRef(class_name()));
}


llvm::StructType* ClassLowerer::GetOrCreateVTableStruct() {
  TypeConverter tc(xylo_context(), llvm_context());

  Vector<llvm::Type*> vtable_entries;
  vtable_entries.push_back(tc.PointerType());  // dummy entry

  for (auto super : xylo_nominal()->supers()) {
    vtable_entries.push_back(LoweringNode::GetVTableStruct(super));
  }

  String vtable_name;
  vtable_name += class_name();
  vtable_name += "_vtable";

  return llvm::StructType::create(llvm_context(), tc.ToArrayRef(vtable_entries), tc.ToStringRef(vtable_name));
}


llvm::GlobalVariable* ClassLowerer::CreateVTableGlobal() {
  String constant_name;
  constant_name += class_name();
  constant_name += "_vtable_instance";

  auto linkage = llvm::GlobalValue::ExternalLinkage;
  auto name = TypeConverter::ToStringRef(constant_name);
  return new llvm::GlobalVariable(*llvm_module(), vtable_struct_, true, linkage, nullptr, name);
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

  return llvm::ConstantStruct::get(LoweringNode::GetVTableStruct(super), tc.ToArrayRef(vtable_entries));
}


llvm::Function* ClassLowerer::CreateVTableEntry(MemberInfo* super_method) {
  auto member_it = member_symbols_.find(super_method->name());
  if (member_it != member_symbols_.end()) {
    return GetOrBuildFunction(member_it->second, {});
  } else {
    return GetOrCreateEmbeddedMethodBridge(super_method);
  }
}


llvm::Function* ClassLowerer::GetOrCreateEmbeddedMethodBridge(MemberInfo* super_method) {
  // check cache
  auto bridge_it = bridge_methods_.find(super_method->name());
  if (bridge_it != bridge_methods_.end()) {
    return bridge_it->second;
  }

  TypeConverter tc(xylo_context(), llvm_context());
  auto method_type = super_method->type()->As<FunctionType>();

  // get bridge function type
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
  auto name = tc.ToStringRef(bridge_name);
  auto bridge_type = llvm::FunctionType::get(return_type, param_types, false);
  auto bridge_func = llvm::Function::Create(bridge_type, llvm::Function::ExternalLinkage, name, llvm_module());

  // cache bridge function
  bridge_methods_.emplace(super_method->name(), bridge_func);

  // build bridge function body
  llvm::IRBuilder<> builder(llvm_context());
  auto bb = llvm::BasicBlock::Create(llvm_context(), "entry", bridge_func);
  builder.SetInsertPoint(bb);

  // get embedded method owner pointer
  Vector<NominalSlot*> method_path;
  xylo_nominal()->GetMemberPath(super_method->name(), &method_path);
  xylo_contract(!method_path.empty());

  auto method_info = method_path[0];
  xylo_contract(method_info->kind() == MemberInfo::Kind::kMethod);

  BuildingUtil bu(&builder);
  auto this_ptr = bridge_func->getArg(0);
  auto owner_ptr = bu.MemberPtr(instance_struct_, this_ptr, method_path, 1);  // skip method slot

  // get embedded method
  auto embedded_method = LoweringNode::GetOrBuildMethod(method_info->owner(), super_method->name(), {});

  // build call to embedded method
  Vector<llvm::Value*> arg_values;
  arg_values.push_back(owner_ptr);
  for (size_t i = 1; i < bridge_func->arg_size(); ++i) {
    arg_values.push_back(bridge_func->getArg(i));
  }

  auto return_value = builder.CreateCall(embedded_method, tc.ToArrayRef(arg_values));
  builder.CreateRet(return_value);

  return bridge_func;
}


llvm::Function* ClassLowerer::GetOrBuildMethod(Identifier* name, const TypeVec& type_args) {
  auto member_it = member_symbols_.find(name);
  xylo_contract(member_it != member_symbols_.end());

  auto method_symbol = member_it->second;
  return GetOrBuildFunction(method_symbol, type_args);
}


void ClassLowerer::RegisterMethod(Identifier* name, Symbol* symbol) {
  member_symbols_.emplace(name, symbol);
}


}  // namespace xylo
