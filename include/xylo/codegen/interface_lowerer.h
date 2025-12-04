
#ifndef XYLO_CODEGEN_INTERFACE_LOWERER_H_
#define XYLO_CODEGEN_INTERFACE_LOWERER_H_

#include <utility>

#include "xylo/codegen/codegen_scope.h"
#include "xylo/codegen/module_lowerer.h"
#include "xylo/syntax/ast.h"

namespace xylo {


class InterfaceLowerer : public CodegenScope {
 public:
  InterfaceLowerer(CodegenScope* parent, SubstitutionPtr&& type_env, InterfaceDeclaration* interface_decl) :
      CodegenScope(Kind::kInterface, parent, std::move(type_env)),
      interface_decl_(interface_decl),
      interface_name_(),
      llvm_struct_(nullptr) {}

  ~InterfaceLowerer() = default;

  InterfaceDeclaration* xylo_interface() const { return interface_decl_; }
  xylo::NominalType* xylo_nominal() const { return interface_decl_->symbol()->type()->As<NominalType>(); }
  const String& interface_name() const { return interface_name_; }
  void set_interface_name(String&& name) { interface_name_ = std::move(name); }

  XyloContext* xylo_context() const { return root()->xylo_context(); }
  llvm::LLVMContext& llvm_context() const { return root()->llvm_context(); }

  const String& mangled_name() const override { return interface_name_; }
  int scope_depth() const override { return xylo_interface()->scope()->depth(); }
  llvm::StructType* scope_data_type() const override { return nullptr; }

  llvm::StructType* GetOrCreateVTableStruct();

 protected:
  llvm::StructType* CreateVTableStruct();

 private:
  InterfaceDeclaration* interface_decl_;
  String interface_name_;
  llvm::StructType* llvm_struct_;
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_INTERFACE_LOWERER_H_
