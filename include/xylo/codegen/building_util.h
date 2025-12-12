
#ifndef XYLO_CODEGEN_BUILDING_UTIL_H_
#define XYLO_CODEGEN_BUILDING_UTIL_H_

#include <llvm/IR/IRBuilder.h>

#include "xylo/codegen/lowering_node.h"
#include "xylo/codegen/program_lowerer.h"
#include "xylo/syntax/type.h"

namespace xylo {


class BuildingUtil {
 public:
  explicit BuildingUtil(LoweringNode* lowerer, llvm::IRBuilder<>* builder) :
      lowerer_(lowerer),
      builder_(builder) {}

  ~BuildingUtil() = default;

  using SlotVec = Vector<NominalSlot*>;
  llvm::Value* MemberPtr(llvm::StructType* type, llvm::Value* ptr, const SlotVec& path, int skip = 0);

  llvm::Value* AdjustType(llvm::Value* value, xylo::Type* from_type, xylo::Type* to_type);

  llvm::Value* CreateClosureObject(llvm::Value* func_ptr, llvm::Value* env_ptr);
  llvm::Value* LoadFunctionPtrFromClosureObject(llvm::Value* closure_obj);
  llvm::Value* LoadEnvironmentPtrFromClosureObject(llvm::Value* closure_obj);

  llvm::Value* CreateInterfaceFatptr(llvm::Value* obj_ptr, llvm::Value* vtable_ptr);
  llvm::Value* LoadObjectPtrFromInterfaceFatptr(llvm::Value* fatptr);
  llvm::Value* LoadVTablePtrFromInterfaceFatptr(llvm::Value* fatptr);

  llvm::Value* CreateHeapAlloc(llvm::Type* type);

 protected:
  LoweringNode* owner_lowerer() const { return lowerer_; }

  XyloContext* xylo_context() const { return lowerer_->root()->xylo_context(); }
  llvm::Module* llvm_module() const { return lowerer_->root()->llvm_module(); }
  llvm::LLVMContext& llvm_context() const { return lowerer_->root()->llvm_context(); }

  llvm::IntegerType* size_type() const { return lowerer_->root()->size_type(); }
  llvm::StructType* closure_object_type() const { return lowerer_->root()->closure_object_type(); }
  llvm::StructType* interface_fatptr_type() const { return lowerer_->root()->interface_fatptr_type(); }
  llvm::Function* xylo_malloc() const { return lowerer_->root()->xylo_malloc(); }
  llvm::Value* null_ptr() const { return lowerer_->root()->null_ptr(); }

 private:
  LoweringNode* lowerer_;
  llvm::IRBuilder<>* builder_;
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_BUILDING_UTIL_H_
