
#ifndef XYLO_CODEGEN_INTERFACE_LOWERER_H_
#define XYLO_CODEGEN_INTERFACE_LOWERER_H_

#include <utility>

#include "xylo/codegen/declaration_lowerer.h"

namespace xylo {


class InterfaceLowerer : public DeclarationLowerer {
 public:
  InterfaceLowerer(LoweringNode* parent, SubstitutionPtr&& subst, InterfaceDeclaration* interface_decl) :
      DeclarationLowerer(Kind::kInterface, parent, std::move(subst)),
      interface_decl_(interface_decl),
      interface_name_(),
      vtable_struct_(nullptr) {}

  ~InterfaceLowerer() = default;

  InterfaceDeclaration* xylo_interface() const { return interface_decl_; }
  xylo::NominalType* xylo_nominal() const { return interface_decl_->symbol()->type()->As<NominalType>(); }
  const String& interface_name() const { return interface_name_; }
  void set_interface_name(String&& name) { interface_name_ = std::move(name); }

  const String& mangled_name() const override { return interface_name_; }
  int scope_depth() const override { xylo_unreachable(); }
  llvm::StructType* scope_data_type() const override { xylo_unreachable(); }

  llvm::StructType* GetOrCreateVTableStruct();

 protected:
  llvm::StructType* CreateVTableStruct();

 private:
  InterfaceDeclaration* interface_decl_;
  String interface_name_;
  llvm::StructType* vtable_struct_;
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_INTERFACE_LOWERER_H_
