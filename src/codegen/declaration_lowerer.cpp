
#include "xylo/codegen/declaration_lowerer.h"

#include "xylo/codegen/building_util.h"
#include "xylo/codegen/type_converter.h"

namespace xylo {


llvm::Type* DeclarationLowerer::ZonkAndConvert(xylo::Type* type, bool function_as_pointer) {
  TypeArena arena;
  auto zonked_type = type->Zonk(subst(), false, &arena);

  TypeConverter type_converter(xylo_context(), llvm_context());
  return type_converter.Convert(zonked_type, function_as_pointer);
}


}  // namespace xylo
