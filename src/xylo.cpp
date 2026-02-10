
#include <gc/gc.h>

#include <iostream>

#include "xylo/codegen/compiler.h"
#include "xylo/syntax/context.h"
#include "xylo/syntax/file.h"
#include "xylo/syntax/flow_analyzer.h"
#include "xylo/syntax/inferencer.h"
#include "xylo/syntax/lexer.h"
#include "xylo/syntax/parser.h"
#include "xylo/syntax/resolver.h"
#include "xylo/util/finally.h"


static void DumpDeclarations(xylo::FileAST* file_ast);
static void DumpDeclarations(int indent, xylo::ClassDeclaration* clazz, xylo::TypePrinter::NameMap* name_map);
static void DumpDelarations(int indent, xylo::Block* block, xylo::TypePrinter::NameMap* name_map);
static void DumpDeclaration(int indent, xylo::Declaration* decl, xylo::TypePrinter::NameMap* name_map = nullptr);

static void DumpDeclarations(xylo::FileAST* file_ast) {
#if 0
  xylo::TypePrinter::NameMap name_map;
  for (auto& decl : file_ast->declarations()) {
    DumpDeclaration(0, decl.get(), &name_map);
  }
#else
  for (auto& decl : file_ast->declarations()) {
    DumpDeclaration(0, decl.get());
  }
#endif
}

static void DumpDelarations(int indent, xylo::Block* block, xylo::TypePrinter::NameMap* name_map) {
  for (auto& decl : block->declarations()) {
    DumpDeclaration(indent, decl.get(), name_map);
  }
}

static void DumpDeclarations(int indent, xylo::InterfaceDeclaration* interface, xylo::TypePrinter::NameMap* name_map) {
  for (auto& decl : interface->methods()) {
    DumpDeclaration(indent, decl.get(), name_map);
  }
}

static void DumpDeclarations(int indent, xylo::ClassDeclaration* clazz, xylo::TypePrinter::NameMap* name_map) {
  for (auto& decl : clazz->declarations()) {
    DumpDeclaration(indent, decl.get(), name_map);
  }
}

static void DumpDeclaration(int indent, xylo::Declaration* decl, xylo::TypePrinter::NameMap* name_map) {
  for (int i = 0; i < indent; ++i) {
    std::cout << "  ";
  }

  xylo::TypePrinter printer({.verbose = true});
  std::cout << decl->symbol()->name()->str().cpp_view() << ": ";
  if (name_map != nullptr) {
    std::cout << printer(decl->symbol()->type(), name_map);
  } else {
    std::cout << printer(decl->symbol()->type());
  }
  std::cout << std::endl;

  switch (decl->kind()) {
    case xylo::Declaration::Kind::kInterface: {
      auto interface = decl->As<xylo::InterfaceDeclaration>();
      DumpDeclarations(indent + 1, interface, name_map);
      break;
    }

    case xylo::Declaration::Kind::kClass: {
      auto clazz = decl->As<xylo::ClassDeclaration>();
      DumpDeclarations(indent + 1, clazz, name_map);
      break;
    }

    case xylo::Declaration::Kind::kFunction: {
      auto func_decl = decl->As<xylo::FunctionDeclaration>();
      if (func_decl->func()->body() != nullptr) {
        DumpDelarations(indent + 1, func_decl->func()->body(), name_map);
      }
      break;
    }
  }
}


static void PrintError(const xylo::DiagnosticReporter& error_reporter) {
  for (auto& error : error_reporter.diagnostics()) {
    std::cerr << error << std::endl;
    error.position.Print(std::cerr);
  }
}


int main(int argc, char** argv) {
  GC_INIT();

  if (argc < 2) {
    std::cerr << "Usage: xylo <file>" << std::endl;
    return 1;
  }
  const char* source_file = argv[1];

  xylo::SourceFile source(source_file);
  if (!source.LoadContent()) {
    std::cerr << "Error loading source file: " << source.path() << std::endl;
    return 1;
  }

  xylo::XyloContext context;
  xylo::Lexer lexer(&context, &source);
  xylo::Parser parser(&context, &lexer);

  auto file_ast = parser.ParseFile();
  if (parser.has_diagnostics()) {
    PrintError(parser);
    return 1;
  }
  context.root_scope()->Tour();

  xylo::Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  if (resolver.has_diagnostics()) {
    PrintError(resolver);
    return 1;
  }

  xylo::Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  if (inferencer.has_diagnostics()) {
    PrintError(inferencer);
    return 1;
  }

  xylo::FlowAnalyzer flow_analyzer(&context);
  flow_analyzer.VisitFileAST(file_ast.get());
  if (flow_analyzer.has_diagnostics()) {
    PrintError(flow_analyzer);
    return 1;
  }

  std::cout << "syntax checking completed successfully." << std::endl;
  std::cout << std::endl;

  DumpDeclarations(file_ast.get());
  std::cout << std::endl;

  xylo::Compiler compiler(&context);
  auto exe = compiler.Compile(file_ast.get(), true);
  if (compiler.has_diagnostics()) {
    PrintError(compiler);
    return 1;
  }

  auto result = exe->Run();
  std::cout << std::endl;
  std::cout << "Program exited with code: " << result << std::endl;

  return 0;
}
