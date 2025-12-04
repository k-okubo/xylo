
#include "xylo/codegen/module_lowerer.h"

#include <llvm/IR/IRBuilder.h>

#include <vector>

namespace xylo {


void ModuleLowerer::RegisterTopLevelDecls(FileAST* file_ast) {
  for (auto& decl : file_ast->declarations()) {
    RegisterDeclaration(decl.get());
  }
}


void ModuleLowerer::BuildEntryPoint(const llvm::Twine& name, Symbol* main_symbol, const Vector<Type*>& main_type_args) {
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


void ModuleLowerer::RegisterInterfaceLowerer(NominalType* type, InterfaceLowerer* lowerer) {
  interface_lowerers_.emplace(type, lowerer);
}


InterfaceLowerer* ModuleLowerer::GetInterfaceLowerer(NominalType* type) {
  auto it = interface_lowerers_.find(type);
  if (it != interface_lowerers_.end()) {
    return it->second;
  } else {
    return nullptr;
  }
}


void ModuleLowerer::RegisterClassLowerer(NominalType* type, ClassLowerer* lowerer) {
  class_lowerers_.emplace(type, lowerer);
}


ClassLowerer* ModuleLowerer::GetClassLowerer(NominalType* type) {
  auto it = class_lowerers_.find(type);
  if (it != class_lowerers_.end()) {
    return it->second;
  } else {
    return nullptr;
  }
}


llvm::IntegerType* ModuleLowerer::size_type() {
  const auto& DL = llvm_module()->getDataLayout();
  return llvm::IntegerType::get(llvm_context(), DL.getPointerSizeInBits());
}


llvm::StructType* ModuleLowerer::closure_ptr_type() {
  if (closure_ptr_type_ != nullptr) {
    return closure_ptr_type_;
  }

  auto ptr_type = llvm::PointerType::getUnqual(llvm_context());
  closure_ptr_type_ = llvm::StructType::create(llvm_context(), {ptr_type, ptr_type}, "ClosurePointer");
  return closure_ptr_type_;
}


llvm::StructType* ModuleLowerer::interface_ptr_type() {
  if (interface_ptr_type_ != nullptr) {
    return interface_ptr_type_;
  }

  auto ptr_type = llvm::PointerType::getUnqual(llvm_context());
  interface_ptr_type_ = llvm::StructType::create(llvm_context(), {ptr_type, ptr_type}, "InterfacePointer");
  return interface_ptr_type_;
}


llvm::Function* ModuleLowerer::xylo_malloc() {
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
