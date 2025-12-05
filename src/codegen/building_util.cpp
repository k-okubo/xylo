
#include "xylo/codegen/building_util.h"

#include "xylo/codegen/type_converter.h"

namespace xylo {


llvm::Value* BuildingUtil::MemberPtr(llvm::StructType* type, llvm::Value* ptr, const SlotVec& path, int skip) {
  xylo_contract(skip >= 0);
  if (static_cast<int>(path.size()) <= skip) {
    return ptr;
  }

  Vector<llvm::Value*> indices;
  indices.push_back(builder_->getInt64(0));

  // reverse path to build GEP
  for (int i = path.size() - 1; i >= skip; --i) {
    auto slot = path[i];
    indices.push_back(builder_->getInt32(slot->index() + 1));  // +1 to skip env pointer
  }

  return builder_->CreateInBoundsGEP(type, ptr, TypeConverter::ToArrayRef(indices));
}


}  // namespace xylo
