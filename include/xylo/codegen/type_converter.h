
#ifndef XYLO_CODEGEN_TYPE_CONVERTER_H_
#define XYLO_CODEGEN_TYPE_CONVERTER_H_

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "xylo/syntax/context.h"
#include "xylo/syntax/type.h"


class TypeConverter {
 public:
  // NOLINTNEXTLINE(runtime/references)
  explicit TypeConverter(xylo::XyloContext* xylo_context, llvm::LLVMContext& llvm_context) :
      xylo_context_(xylo_context),
      llvm_context_(llvm_context) {}

  ~TypeConverter() = default;

  llvm::Type* Convert(xylo::Type* type, bool function_as_pointer);

  llvm::Type* ConvertNominalType(xylo::NominalType* type);
  llvm::Type* ConvertFunctionType(xylo::FunctionType* type, bool as_pointer);

  llvm::StructType* CreateStructType(xylo::NominalType* type);

  template <typename T>
  static llvm::ArrayRef<T> ToArrayRef(const xylo::Vector<T>& vec) {
    return llvm::ArrayRef<T>(vec.data(), vec.size());
  }

 private:
  xylo::XyloContext* xylo_context_;
  llvm::LLVMContext& llvm_context_;
};


#endif  // XYLO_CODEGEN_TYPE_CONVERTER_H_
