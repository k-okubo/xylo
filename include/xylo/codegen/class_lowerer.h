
#ifndef XYLO_CODEGEN_CLASS_LOWERER_H_
#define XYLO_CODEGEN_CLASS_LOWERER_H_

#include <utility>

#include "xylo/codegen/declaration_lowerer.h"

namespace xylo {


class ClassLowerer : public DeclarationLowerer {
 public:
  ClassLowerer(LoweringNode* parent, SubstitutionPtr&& subst, ClassDeclaration* class_decl) :
      DeclarationLowerer(Kind::kClass, parent, std::move(subst)),
      class_decl_(class_decl),
      class_name_(),
      instance_struct_(nullptr),
      vtable_struct_(nullptr),
      vtable_global_(nullptr),
      member_symbols_() {}

  ~ClassLowerer() = default;

  ClassDeclaration* xylo_class() const { return class_decl_; }
  xylo::NominalType* xylo_nominal() const { return class_decl_->symbol()->type()->As<NominalType>(); }
  const String& class_name() const { return class_name_; }
  void set_class_name(String&& name) { class_name_ = std::move(name); }

  const String& mangled_name() const override { return class_name_; }
  Scope* scope() const override { return xylo_class()->inner_scope(); }
  llvm::StructType* scope_data_type() const override { return instance_struct_; }

  llvm::StructType* GetOrCreateInstanceStruct();
  llvm::Function* GetOrBuildMethod(Identifier* name, const TypeVec& type_args);

  llvm::StructType* GetVTableStruct() const { return vtable_struct_; }
  llvm::Value* GetVTablePtr() const { return vtable_global_; }

 protected:
  llvm::StructType* CreateInstanceStruct();
  llvm::StructType* CreateVTableStruct();
  llvm::GlobalVariable* CreateVTableGlobal();
  llvm::Constant* CreateVTableConstant();
  llvm::Constant* CreateVTableConstant(NominalType* super);
  llvm::Function* CreateVTableEntry(MemberInfo* super_method);

  struct BridgeInfo {
    String bridge_name;
    Vector<NominalSlot*>& target_path;
    llvm::Function* target_func;
    xylo::FunctionType* target_type;
    xylo::FunctionType* bridge_type;
  };
  llvm::Function* CreateMethodBridge(const BridgeInfo& bridge_info);

  void RegisterMethod(Identifier* name, Symbol* symbol);

 private:
  ClassDeclaration* class_decl_;
  String class_name_;
  llvm::StructType* instance_struct_;
  llvm::StructType* vtable_struct_;
  llvm::GlobalVariable* vtable_global_;

  Map<Identifier*, Symbol*> member_symbols_;
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_CLASS_LOWERER_H_
