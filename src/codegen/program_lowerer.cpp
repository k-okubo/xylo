
#include "xylo/codegen/program_lowerer.h"

#include <llvm/IR/IRBuilder.h>

#include <vector>

namespace xylo {


void ProgramLowerer::RegisterTopLevelDecls(FileAST* file_ast) {
  for (auto& decl : file_ast->declarations()) {
    RegisterDeclaration(decl.get());
  }
}


void ProgramLowerer::BuildEntryPoint(const llvm::Twine& name, Symbol* main_symbol,
                                     const Vector<Type*>& main_type_args) {
  std::vector<llvm::Type*> param_types;
  auto entry_type = llvm::FunctionType::get(llvm::IntegerType::getInt64Ty(llvm_context()), param_types, false);
  auto entry_func = llvm::Function::Create(entry_type, llvm::Function::ExternalLinkage, name, llvm_module());

  llvm::IRBuilder<> builder(llvm_context());
  auto bb = llvm::BasicBlock::Create(llvm_context(), "entry", entry_func);
  builder.SetInsertPoint(bb);

  auto main_func = GetOrBuildFunction(main_symbol, main_type_args);

  std::vector<llvm::Value*> args;
  args.push_back(llvm::Constant::getNullValue(main_func->getFunctionType()->getParamType(0)));  // env pointer
  llvm::Value* ret_value = builder.CreateCall(main_func, args);

  if (!main_func->getReturnType()->isIntegerTy(64)) {
    ret_value = builder.getInt64(0);
  }
  builder.CreateRet(ret_value);
}


llvm::IntegerType* ProgramLowerer::size_type() {
  const auto& DL = llvm_module()->getDataLayout();
  return llvm::IntegerType::get(llvm_context(), DL.getPointerSizeInBits());
}


llvm::StructType* ProgramLowerer::closure_object_type() {
  if (closure_object_type_ != nullptr) {
    return closure_object_type_;
  }

  auto ptr_type = llvm::PointerType::getUnqual(llvm_context());
  closure_object_type_ = llvm::StructType::create(llvm_context(), {ptr_type, ptr_type}, "ClosureObject");
  return closure_object_type_;
}


llvm::StructType* ProgramLowerer::interface_fatptr_type() {
  if (interface_fatptr_type_ != nullptr) {
    return interface_fatptr_type_;
  }

  auto ptr_type = llvm::PointerType::getUnqual(llvm_context());
  interface_fatptr_type_ = llvm::StructType::create(llvm_context(), {ptr_type, ptr_type}, "InterfaceFatPointer");
  return interface_fatptr_type_;
}


llvm::Function* ProgramLowerer::xylo_malloc() {
  if (xylo_malloc_ != nullptr) {
    return xylo_malloc_;
  }

  auto ptr_type = llvm::PointerType::getUnqual(llvm_context());
  auto malloc_type = llvm::FunctionType::get(ptr_type, {size_type()}, false);
  auto malloc_func = llvm::Function::Create(malloc_type, llvm::Function::ExternalLinkage, "xylo_malloc", llvm_module());
  xylo_malloc_ = malloc_func;

  return malloc_func;
}


}  // namespace xylo
