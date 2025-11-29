
#include "xylo/codegen/compiler.h"

#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Utils/Mem2Reg.h>

#include <cstddef>

#include "xylo/codegen/module_lowerer.h"
#include "xylo/runtime/memory.h"


namespace xylo {


static constexpr auto kEntryPointName = "__entry_point";

static void RegisterSystemFunctions(llvm::orc::LLJIT* jit) {
  llvm::orc::MangleAndInterner mangle(jit->getExecutionSession(), jit->getDataLayout());
  llvm::orc::SymbolMap symbols;

#define SYMBOL_DEF(func)                                                                         \
  do {                                                                                           \
    auto addr = llvm::orc::ExecutorAddr::fromPtr(&func);                                         \
    symbols[mangle(#func)] = llvm::orc::ExecutorSymbolDef(addr, llvm::JITSymbolFlags::Exported); \
  } while (0)

  SYMBOL_DEF(xylo_malloc);

#undef SYMBOL_DEF

  llvm::cantFail(jit->getMainJITDylib().define(llvm::orc::absoluteSymbols(symbols)));
}


static void OptimizeModule(llvm::Module* M, llvm::OptimizationLevel O) {
  llvm::PassBuilder PB;

  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;

  FAM.registerPass([&] {
    return PB.buildDefaultAAPipeline();
  });
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  llvm::ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(O);

  MPM.run(*M, MAM);
}


static void LightOptimizeModule(llvm::Module* M) {
  llvm::PassBuilder PB;

  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;

  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  llvm::FunctionPassManager FPM;
  FPM.addPass(llvm::PromotePass());  // mem2reg
  // FPM.addPass(llvm::InstCombinePass());

  llvm::ModulePassManager MPM;
  MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));

  MPM.run(*M, MAM);
}


EntryPoint Compiler::Compile(FileAST* file_ast, bool print_module) {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  auto Err = llvm::ExitOnError();
  auto jit = Err(llvm::orc::LLJITBuilder().create());
  RegisterSystemFunctions(jit.get());

  // prepare module
  auto llvm_context = llvm::orc::ThreadSafeContext(std::make_unique<llvm::LLVMContext>());
  auto module = std::make_unique<llvm::Module>("main_module", *llvm_context.getContext());
  module->setDataLayout(jit->getDataLayout());
  module->setTargetTriple(jit->getTargetTriple().str());

  // prepare lowerer
  ModuleLowerer module_lowerer(xylo_context_, module.get());
  module_lowerer.RegisterTopLevelDecls(file_ast);

  // build entry point
  auto main_symbol = FindMainSymbol(file_ast);
  if (main_symbol == nullptr) {
    return nullptr;
  }
  Vector<Type*> main_type_args;
  if (!CreateMainTypeArgs(main_symbol, &main_type_args)) {
    return nullptr;
  }
  module_lowerer.BuildEntryPoint(kEntryPointName, main_symbol, main_type_args);

  // optimize
  if (false) {
    OptimizeModule(module.get(), llvm::OptimizationLevel::O2);
  } else if (true) {
    LightOptimizeModule(module.get());
  }

  if (print_module) {
    module->print(llvm::outs(), nullptr);
  }

  if (llvm::verifyModule(*module, &llvm::errs())) {
    llvm::errs() << "Module verification failed\n";
    xylo_unreachable();
  }

  Err(jit->addIRModule(llvm::orc::ThreadSafeModule(std::move(module), llvm_context)));

  auto entry_symbol = Err(jit->lookup(kEntryPointName));
  return entry_symbol.toPtr<EntryPoint>();
}


Symbol* Compiler::FindMainSymbol(FileAST* file_ast) {
  auto main_ident = xylo_context_->InternIdentifier("main");
  for (auto& decl : file_ast->declarations()) {
    if (decl->symbol()->name() == main_ident) {
      return decl->symbol();
    }
  }

  ReportError(SourceRange(), "cannot find 'main' function");
  return nullptr;
}


bool Compiler::CreateMainTypeArgs(Symbol* main_symbol, Vector<Type*>* out_type_args) {
  xylo_contract(out_type_args->empty());

  TypeSink allocated;
  Vector<TypeMetavar*> instanciated_vars;
  auto main_func_type = main_symbol->type()->Instantiate(&allocated, &instanciated_vars);

  TupleType param_type;
  TypeMetavar return_type;
  FunctionType main_context_type(false, &param_type, &return_type);

  if (main_func_type->ConstrainSubtypeOf(&main_context_type)) {
    // ok
  } else {
    ReportError(main_symbol->position(), "'main' function must have no parameters");
    return false;
  }

  if (return_type.ConstrainSubtypeOf(xylo_context_->int_type())) {
    // ok
  } else if (return_type.ConstrainSubtypeOf(xylo_context_->unit_type())) {
    // ok
  } else {
    ReportError(main_symbol->position(), "'main' function must have return type 'int' or 'unit'");
    return false;
  }

  Substitution empty_env;
  for (auto* var : instanciated_vars) {
    out_type_args->push_back(var->Zonk(&empty_env, false, &allocated));
  }

  return true;
}


}  // namespace xylo
