
#ifndef XYLO_CODEGEN_DECLARATION_LOWERER_H_
#define XYLO_CODEGEN_DECLARATION_LOWERER_H_

#include <utility>

#include "xylo/codegen/lowering_node.h"
#include "xylo/codegen/program_lowerer.h"

namespace xylo {


class DeclarationLowerer : public LoweringNode {
 protected:
  DeclarationLowerer(LoweringNode::Kind kind, LoweringNode* parent, SubstitutionPtr&& subst) :
      LoweringNode(kind, parent, std::move(subst)) {}

  XyloContext* xylo_context() const { return root()->xylo_context(); }
  llvm::Module* llvm_module() const { return root()->llvm_module(); }
  llvm::LLVMContext& llvm_context() const { return root()->llvm_context(); }

  llvm::IntegerType* size_type() const { return root()->size_type(); }
  llvm::StructType* closure_object_type() const { return root()->closure_object_type(); }
  llvm::StructType* interface_fatptr_type() const { return root()->interface_fatptr_type(); }
  llvm::Function* xylo_malloc() const { return root()->xylo_malloc(); }
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_DECLARATION_LOWERER_H_
