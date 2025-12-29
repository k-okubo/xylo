
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

  // create struct
  instance_struct_ = CreateInstanceStruct();
  vtable_struct_ = CreateVTableStruct();

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
    struct_entries[index + 1] = ZonkAndConvert(field->type(), true);
  }

  return llvm::StructType::create(llvm_context(), tc.ToArrayRef(struct_entries), tc.ToStringRef(class_name()));
}


llvm::StructType* ClassLowerer::CreateVTableStruct() {
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
  auto method_name = super_method->name();
  auto super_method_type = super_method->type()->As<FunctionType>();

  // get method path
  Vector<NominalSlot*> method_path;
  xylo_nominal()->GetMemberPath(method_name, &method_path);
  xylo_contract(!method_path.empty());

  auto method_info = method_path[0];
  xylo_contract(method_info->kind() == MemberInfo::Kind::kMethod);
  auto method_type = method_info->As<MemberInfo>()->type();

  // get method func
  TypeArena arena;
  auto instantiated_method_type = method_type->Instantiate(&arena);
  xylo_check(instantiated_method_type->ConstrainSubtypeOf(super_method_type));
  auto instantiated_info = instantiated_method_type->instantiated_info();
  auto method_func = LoweringNode::GetOrBuildMethod(method_info->owner(), method_name, instantiated_info);
  auto zonked_method_type = instantiated_method_type->Zonk(subst(), false, &arena)->As<FunctionType>();

  if (method_info->owner() == xylo_nominal() && zonked_method_type->equals(super_method_type)) {
    return method_func;
  }

  String bridge_name;
  bridge_name += class_name();
  bridge_name += "__";
  bridge_name += method_name->str();
  bridge_name += "_for_";
  bridge_name += super_method->owner()->name()->str();

  BridgeInfo bridge_info{
    .bridge_name = std::move(bridge_name),
    .target_path = method_path,
    .target_func = method_func,
    .target_type = zonked_method_type,
    .bridge_type = super_method_type,
  };

  return CreateMethodBridge(bridge_info);
}


llvm::Function* ClassLowerer::CreateMethodBridge(const BridgeInfo& bridge_info) {
  TypeConverter tc(xylo_context(), llvm_context());

  // get bridge function type
  std::vector<llvm::Type*> param_types;
  param_types.push_back(tc.PointerType());  // this pointer
  for (auto& param_type : bridge_info.bridge_type->params_type()->elements()) {
    param_types.push_back(tc.Convert(param_type, true));
  }
  auto return_type = tc.Convert(bridge_info.bridge_type->return_type(), true);

  // create bridge function
  auto name = tc.ToStringRef(bridge_info.bridge_name);
  auto bridge_type = llvm::FunctionType::get(return_type, param_types, false);
  auto bridge_func = llvm::Function::Create(bridge_type, llvm::Function::ExternalLinkage, name, llvm_module());

  // build bridge function body
  llvm::IRBuilder<> builder(llvm_context());
  auto bb = llvm::BasicBlock::Create(llvm_context(), "entry", bridge_func);
  builder.SetInsertPoint(bb);

  // get method owner pointer
  BuildingUtil bu(this, &builder);
  auto this_ptr = bridge_func->getArg(0);
  auto owner_ptr = bu.MemberPtr(instance_struct_, this_ptr, bridge_info.target_path, 1);  // skip method slot

  // call target method
  Vector<llvm::Value*> arg_values;
  arg_values.push_back(owner_ptr);
  for (size_t i = 1; i < bridge_func->arg_size(); ++i) {
    auto arg_type = bridge_info.bridge_type->params_type()->elements()[i - 1];
    auto param_type = bridge_info.target_type->params_type()->elements()[i - 1];

    auto arg_value = bridge_func->getArg(i);
    arg_values.push_back(bu.AdjustType(arg_value, arg_type, param_type));
  }

  auto target_return_type = bridge_info.target_type->return_type();
  auto bridge_return_type = bridge_info.bridge_type->return_type();
  auto return_value = builder.CreateCall(bridge_info.target_func, tc.ToArrayRef(arg_values));
  builder.CreateRet(bu.AdjustType(return_value, target_return_type, bridge_return_type));

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
