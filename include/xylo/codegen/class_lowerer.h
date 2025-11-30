
#ifndef XYLO_CODEGEN_CLASS_LOWERER_H_
#define XYLO_CODEGEN_CLASS_LOWERER_H_

#include <utility>

#include "xylo/codegen/codegen_scope.h"
#include "xylo/codegen/module_lowerer.h"
#include "xylo/syntax/ast.h"

namespace xylo {


class ClassLowerer : public CodegenScope {
 public:
  ClassLowerer(CodegenScope* parent, SubstitutionPtr&& type_env, ClassDeclaration* class_decl) :
      CodegenScope(Kind::kClass, parent, std::move(type_env)),
      class_decl_(class_decl),
      class_name_(),
      llvm_struct_(nullptr),
      member_symbols_() {}

  ~ClassLowerer() = default;

  ClassDeclaration* xylo_class() const { return class_decl_; }
  xylo::NominalType* xylo_nominal() const { return class_decl_->symbol()->type()->As<NominalType>(); }
  const String& class_name() const { return class_name_; }
  void set_class_name(String&& name) { class_name_ = std::move(name); }

  XyloContext* xylo_context() const { return root()->xylo_context(); }
  llvm::LLVMContext& llvm_context() const { return root()->llvm_context(); }

  const String& mangled_name() const override { return class_name_; }
  int scope_depth() const override { return xylo_class()->scope()->depth(); }
  llvm::StructType* scope_data_type() const override { return llvm_struct_; }

  Symbol* class_symbol() const { return class_decl_->symbol(); }

  llvm::StructType* GetOrCreate();
  llvm::Function* GetOrBuildMethod(Identifier* name, const Vector<Type*>& type_args);

 protected:
  llvm::StructType* CreateStruct();
  void RegisterMethod(Identifier* name, Symbol* symbol);

 private:
  ClassDeclaration* class_decl_;
  String class_name_;
  llvm::StructType* llvm_struct_;

  Map<Identifier*, Symbol*> member_symbols_;
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_CLASS_LOWERER_H_
