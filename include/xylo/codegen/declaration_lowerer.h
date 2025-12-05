
#ifndef XYLO_CODEGEN_DECLARATION_LOWERER_H_
#define XYLO_CODEGEN_DECLARATION_LOWERER_H_

#include <utility>

#include "xylo/codegen/lowering_node.h"
#include "xylo/codegen/program_lowerer.h"

namespace xylo {


class DeclarationLowerer : public LoweringNode {
 protected:
  DeclarationLowerer(LoweringNode::Kind kind, LoweringNode* parent, SubstitutionPtr&& type_env) :
      LoweringNode(kind, parent, std::move(type_env)) {}

  XyloContext* xylo_context() const { return root()->xylo_context(); }
  llvm::Module* llvm_module() const { return root()->llvm_module(); }
  llvm::LLVMContext& llvm_context() const { return root()->llvm_context(); }

  llvm::IntegerType* size_type() const { return root()->size_type(); }
  llvm::StructType* closure_ptr_type() const { return root()->closure_ptr_type(); }
  llvm::StructType* interface_ptr_type() const { return root()->interface_ptr_type(); }
  llvm::Function* xylo_malloc() const { return root()->xylo_malloc(); }
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_DECLARATION_LOWERER_H_
