
#ifndef XYLO_CODEGEN_PROGRAM_LOWERER_H_
#define XYLO_CODEGEN_PROGRAM_LOWERER_H_

#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>

#include "xylo/codegen/lowering_node.h"
#include "xylo/syntax/context.h"

namespace xylo {


class ProgramLowerer : public LoweringNode {
 public:
  ProgramLowerer(XyloContext* xylo_context, llvm::Module* llvm_module) :
      LoweringNode(Kind::kProgram, this),
      xylo_context_(xylo_context),
      llvm_module_(llvm_module),
      closure_object_type_(nullptr),
      interface_fatptr_type_(nullptr),
      xylo_malloc_(nullptr) {}

  ~ProgramLowerer() = default;

  XyloContext* xylo_context() const { return xylo_context_; }
  llvm::Module* llvm_module() const { return llvm_module_; }
  llvm::LLVMContext& llvm_context() const { return llvm_module()->getContext(); }

  llvm::IntegerType* size_type();
  llvm::StructType* closure_object_type();
  llvm::StructType* interface_fatptr_type();
  llvm::Function* xylo_malloc();
  llvm::Value* null_ptr() const { return llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(llvm_context())); }

  const String& mangled_name() const override {
    static String name("XY");
    return name;
  }

  Scope* scope() const override { return xylo_context()->root_scope(); }
  llvm::StructType* scope_data_type() const override { return nullptr; }

  void RegisterTopLevelDecls(FileAST* file_ast);
  void BuildEntryPoint(const llvm::Twine& name, Symbol* main_symbol, const Vector<Type*>& main_type_args);

 private:
  XyloContext* xylo_context_;
  llvm::Module* llvm_module_;

  llvm::StructType* closure_object_type_;
  llvm::StructType* interface_fatptr_type_;
  llvm::Function* xylo_malloc_;
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_PROGRAM_LOWERER_H_
