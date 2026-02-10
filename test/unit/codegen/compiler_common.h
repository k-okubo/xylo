
#ifndef TEST_UNIT_CODEGEN_COMPILER_COMMON_H_
#define TEST_UNIT_CODEGEN_COMPILER_COMMON_H_

#include "xylo/codegen/compiler.h"
#include "xylo/syntax/context.h"
#include "xylo/syntax/flow_analyzer.h"
#include "xylo/syntax/inferencer.h"
#include "xylo/syntax/lexer.h"
#include "xylo/syntax/parser.h"
#include "xylo/syntax/resolver.h"


namespace xylo {


FileASTPtr GetVerifiedAST(XyloContext* context, const char* source) {
  Lexer lexer(context, source);
  Parser parser(context, &lexer);
  auto file_ast = parser.ParseFile();
  xylo_contract(!parser.has_diagnostics());
  context->root_scope()->Tour();

  Resolver resolver(context);
  resolver.VisitFileAST(file_ast.get());
  xylo_contract(!resolver.has_diagnostics());

  Inferencer inferencer(context);
  inferencer.VisitFileAST(file_ast.get());
  xylo_contract(!inferencer.has_diagnostics());

  FlowAnalyzer flow_analyzer(context);
  flow_analyzer.VisitFileAST(file_ast.get());
  xylo_contract(!flow_analyzer.has_diagnostics());

  return file_ast;
}


int64_t CompileAndRun(const char* source) {
  XyloContext context;
  auto file_ast = GetVerifiedAST(&context, source);

  Compiler compiler(&context);
  auto exe = compiler.Compile(file_ast.get());
  xylo_contract(!compiler.has_diagnostics());

  return exe->Run();
}


}  // namespace xylo

#endif  // TEST_UNIT_CODEGEN_COMPILER_COMMON_H_
