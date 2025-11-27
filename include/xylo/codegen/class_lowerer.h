
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
      llvm_struct_(nullptr),
      member_symbols_() {}

  ~ClassLowerer() = default;

  ClassDeclaration* xylo_class() const { return class_decl_; }
  xylo::NominalType* xylo_nominal() const { return class_decl_->symbol()->type()->As<NominalType>(); }

  XyloContext* xylo_context() const { return root()->xylo_context(); }
  llvm::LLVMContext& llvm_context() const { return root()->llvm_context(); }

  int scope_depth() const override { return xylo_class()->symbol()->scope()->depth(); }
  llvm::StructType* scope_data_type() const override { return llvm_struct_; }

  llvm::StructType* GetOrCreateStruct();
  llvm::Function* GetOrBuildMethod(Identifier* name, const Vector<Type*>& type_args);

 protected:
  llvm::StructType* CreateStruct();
  void RegisterMethod(Identifier* name, Symbol* symbol);

 private:
  ClassDeclaration* class_decl_;
  llvm::StructType* llvm_struct_;

  Map<Identifier*, Symbol*> member_symbols_;
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_CLASS_LOWERER_H_
