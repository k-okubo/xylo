
#include "xylo/codegen/building_util.h"

#include "xylo/codegen/class_lowerer.h"
#include "xylo/codegen/type_converter.h"

namespace xylo {


llvm::Value* BuildingUtil::MemberPtr(llvm::StructType* type, llvm::Value* ptr, const SlotVec& path, int skip) {
  xylo_contract(skip >= 0);
  if (static_cast<int>(path.size()) <= skip) {
    return ptr;
  }

  Vector<llvm::Value*> indices;
  indices.push_back(builder_->getInt64(0));

  // reverse path to build GEP
  for (int i = path.size() - 1; i >= skip; --i) {
    auto slot = path[i];
    indices.push_back(builder_->getInt32(slot->index() + 1));  // +1 to skip env pointer
  }

  return builder_->CreateInBoundsGEP(type, ptr, TypeConverter::ToArrayRef(indices));
}


llvm::Value* BuildingUtil::AdjustType(llvm::Value* value, xylo::Type* from_type, xylo::Type* to_type) {
  if (from_type == to_type) {
    return value;
  }

  if (from_type == xylo_context()->int_type() && to_type == xylo_context()->float_type()) {
    return builder_->CreateSIToFP(value, llvm::Type::getDoubleTy(llvm_context()));
  }

  if (from_type->kind() == Type::Kind::kNominal && to_type->kind() == Type::Kind::kNominal) {
    // upcast between nominal types
    auto from_nominal = from_type->As<NominalType>();
    auto to_nominal = to_type->As<NominalType>();

    Vector<NominalSlot*> super_path;
    xylo_check(from_nominal->GetSuperPath(to_nominal, &super_path));
    xylo_contract(!super_path.empty());

    llvm::Value* object_ptr;
    llvm::Value* vtable_ptr;
    llvm::StructType* vtable_struct;

    if (from_nominal->category() == NominalType::Category::kClass) {
      // from_nominal is a class; get vtable from global constant
      auto class_lowerer = owner_lowerer()->GetClassLowerer(from_nominal);
      object_ptr = value;
      vtable_ptr = class_lowerer->GetVTablePtr();
      vtable_struct = class_lowerer->GetVTableStruct();

    } else {
      // from_nominal is an interface; load object and vtable pointers from `value`
      xylo_contract(from_nominal->category() == NominalType::Category::kInterface);
      object_ptr = LoadObjectPtrFromInterfaceFatptr(value);
      vtable_ptr = LoadVTablePtrFromInterfaceFatptr(value);
      vtable_struct = owner_lowerer()->GetVTableStruct(from_nominal);
    }

    // build interface fat pointer with adjusted vtable
    vtable_ptr = MemberPtr(vtable_struct, vtable_ptr, super_path);
    return CreateInterfaceFatptr(object_ptr, vtable_ptr);
  }

  if (from_type->kind() == Type::Kind::kFunction && to_type->kind() == Type::Kind::kFunction) {
    auto from_func_type = from_type->As<FunctionType>();
    auto to_func_type = to_type->As<FunctionType>();

    if (from_func_type->params_type()->equals(to_func_type->params_type()) &&
        from_func_type->return_type()->equals(to_func_type->return_type())) {
      if (from_func_type->is_closure() == to_func_type->is_closure()) {
        // same function type
        return value;
      } else {
        // by type checking, regular function to closure function conversion
        xylo_contract(!from_func_type->is_closure());
        xylo_contract(to_func_type->is_closure());

        return CreateClosureObject(value, null_ptr());
      }
    }

    if (!to_type->is_monotype()) {
      // to_type contains an incomplete type like bottom, meaning this value will never be called
      return value;
    }
  }

#if 0  // enable later if needed
  if (from_type->is_bottom_type()) {
    TypeConverter converter(xylo_context(), llvm_context());
    auto llvm_to_type = converter.Convert(to_type);
    return llvm::UndefValue::get(llvm_to_type);
  }
#endif

  if (to_type->is_top_type()) {
    return value;
  }

  throw std::logic_error("unsupported type adjustment in FunctionLowerer::AdjustType");
}


llvm::Value* BuildingUtil::CreateClosureObject(llvm::Value* func_ptr, llvm::Value* env_ptr) {
  auto closure_ty = closure_object_type();
  auto closure_obj = CreateHeapAlloc(closure_ty);

  auto func_ptr_field = builder_->CreateStructGEP(closure_ty, closure_obj, 0);
  builder_->CreateStore(func_ptr, func_ptr_field);

  auto env_ptr_field = builder_->CreateStructGEP(closure_ty, closure_obj, 1);
  builder_->CreateStore(env_ptr, env_ptr_field);

  return closure_obj;
}


llvm::Value* BuildingUtil::LoadFunctionPtrFromClosureObject(llvm::Value* closure_obj) {
  auto closure_ty = closure_object_type();
  auto func_ptr_field = builder_->CreateStructGEP(closure_ty, closure_obj, 0);
  return builder_->CreateLoad(closure_ty->getElementType(0), func_ptr_field);
}


llvm::Value* BuildingUtil::LoadEnvironmentPtrFromClosureObject(llvm::Value* closure_obj) {
  auto closure_ty = closure_object_type();
  auto env_ptr_field = builder_->CreateStructGEP(closure_ty, closure_obj, 1);
  return builder_->CreateLoad(closure_ty->getElementType(1), env_ptr_field);
}


llvm::Value* BuildingUtil::CreateInterfaceFatptr(llvm::Value* obj_ptr, llvm::Value* vtable_ptr) {
  auto fatptr_ty = interface_fatptr_type();
  auto fatptr = CreateHeapAlloc(fatptr_ty);

  auto obj_ptr_field = builder_->CreateStructGEP(fatptr_ty, fatptr, 0);
  builder_->CreateStore(obj_ptr, obj_ptr_field);

  auto vtable_ptr_field = builder_->CreateStructGEP(fatptr_ty, fatptr, 1);
  builder_->CreateStore(vtable_ptr, vtable_ptr_field);

  return fatptr;
}


llvm::Value* BuildingUtil::LoadObjectPtrFromInterfaceFatptr(llvm::Value* fatptr) {
  auto fatptr_ty = interface_fatptr_type();
  auto obj_ptr_field = builder_->CreateStructGEP(fatptr_ty, fatptr, 0);
  return builder_->CreateLoad(fatptr_ty->getElementType(0), obj_ptr_field);
}


llvm::Value* BuildingUtil::LoadVTablePtrFromInterfaceFatptr(llvm::Value* fatptr) {
  auto fatptr_ty = interface_fatptr_type();
  auto vtable_ptr_field = builder_->CreateStructGEP(fatptr_ty, fatptr, 1);
  return builder_->CreateLoad(fatptr_ty->getElementType(1), vtable_ptr_field);
}


llvm::Value* BuildingUtil::CreateHeapAlloc(llvm::Type* type) {
  auto alloc_size = llvm_module()->getDataLayout().getTypeAllocSize(type);
  auto alloc_size_int = llvm::ConstantInt::get(size_type(), alloc_size);

  auto malloc_func = xylo_malloc();
  auto ptr = builder_->CreateCall(malloc_func, {alloc_size_int});

  return ptr;
}


}  // namespace xylo
