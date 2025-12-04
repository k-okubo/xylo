
#ifndef XYLO_CODEGEN_POINTER_ADJUSTER_H_
#define XYLO_CODEGEN_POINTER_ADJUSTER_H_

#include <llvm/IR/IRBuilder.h>

#include "xylo/syntax/type.h"

namespace xylo {


class PointerAdjuster {
 public:
  explicit PointerAdjuster(llvm::IRBuilder<>* builder) :
      builder_(builder) {}

  ~PointerAdjuster() = default;

  llvm::Value* StructFieldPtr(llvm::StructType* type, llvm::Value* ptr, const Vector<NominalSlot*>& path, int skip = 0);

 private:
  llvm::IRBuilder<>* builder_;
};

}  // namespace xylo

#endif  // XYLO_CODEGEN_POINTER_ADJUSTER_H_
