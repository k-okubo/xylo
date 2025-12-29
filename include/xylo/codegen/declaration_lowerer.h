
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
  llvm::Value* null_ptr() const { return root()->null_ptr(); }

  llvm::Type* ZonkAndConvert(xylo::Type* type, bool function_as_pointer);
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_DECLARATION_LOWERER_H_
