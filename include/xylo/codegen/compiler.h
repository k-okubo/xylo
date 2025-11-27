
#ifndef XYLO_CODEGEN_COMPILER_H_
#define XYLO_CODEGEN_COMPILER_H_

#include "xylo/syntax/ast.h"
#include "xylo/syntax/context.h"
#include "xylo/syntax/diagnostic.h"

namespace xylo {


using EntryPoint = int64_t (*)();

class Compiler : public DiagnosticReporter {
 public:
  explicit Compiler(XyloContext* xylo_context) :
      xylo_context_(xylo_context) {}

  ~Compiler() = default;

  Compiler(const Compiler&) = delete;
  Compiler& operator=(const Compiler&) = delete;

  EntryPoint Compile(FileAST* file_ast, bool print_module = false);

 protected:
  Symbol* FindMainSymbol(FileAST* file_ast);
  bool CreateMainTypeArgs(Symbol* main_symbol, Vector<Type*>* out_type_args);

 private:
  XyloContext* xylo_context_;
};

}  // namespace xylo

#endif  // XYLO_CODEGEN_COMPILER_H_
