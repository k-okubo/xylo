
#ifndef XYLO_CODEGEN_BUILDING_UTIL_H_
#define XYLO_CODEGEN_BUILDING_UTIL_H_

#include <llvm/IR/IRBuilder.h>

#include "xylo/syntax/type.h"

namespace xylo {


class BuildingUtil {
 public:
  explicit BuildingUtil(llvm::IRBuilder<>* builder) :
      builder_(builder) {}

  ~BuildingUtil() = default;

  using SlotVec = Vector<NominalSlot*>;
  llvm::Value* MemberPtr(llvm::StructType* type, llvm::Value* ptr, const SlotVec& path, int skip = 0);

 private:
  llvm::IRBuilder<>* builder_;
};

}  // namespace xylo

#endif  // XYLO_CODEGEN_BUILDING_UTIL_H_
