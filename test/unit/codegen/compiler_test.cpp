
#include "xylo/codegen/compiler.h"

#include <gtest/gtest.h>

#include "./compiler_common.h"

namespace xylo {

static_assert(!std::copyable<Compiler>);
static_assert(!std::movable<Compiler>);


TEST(CompilerTest, TernaryExpression) {
  auto source = R"(
    def main() {
      return 1 < 2 ? 42 : 99
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(CompilerTest, MissingMainFunction) {
  auto source = R"(
    def foo() {
      return 0
    }
  )";

  XyloContext context;
  auto file_ast = GetVerifiedAST(&context, source);

  Compiler compiler(&context);
  compiler.Compile(file_ast.get());
  ASSERT_TRUE(compiler.has_diagnostics());
  EXPECT_EQ(compiler.diagnostics().size(), 1);
  EXPECT_EQ(compiler.diagnostics()[0].message, "cannot find 'main' function");
  EXPECT_EQ(compiler.diagnostics()[0].position.start.line, 0);
  EXPECT_EQ(compiler.diagnostics()[0].position.start.column, 0);
}


TEST(CompilerTest, MainFunction_InvalidParamType) {
  auto source = R"(
    def main(x: int) {
      return 0
    }
  )";

  XyloContext context;
  auto file_ast = GetVerifiedAST(&context, source);

  Compiler compiler(&context);
  compiler.Compile(file_ast.get());
  ASSERT_TRUE(compiler.has_diagnostics());
  EXPECT_EQ(compiler.diagnostics().size(), 1);
  EXPECT_EQ(compiler.diagnostics()[0].message, "'main' function must have no parameters");
  EXPECT_EQ(compiler.diagnostics()[0].position.start.line, 2);
  EXPECT_EQ(compiler.diagnostics()[0].position.start.column, 9);
}


TEST(CompilerTest, MainFunction_InvalidReturnType) {
  auto source = R"(
    def main() : float {
      return 0.0
    }
  )";

  XyloContext context;
  auto file_ast = GetVerifiedAST(&context, source);

  Compiler compiler(&context);
  compiler.Compile(file_ast.get());
  ASSERT_TRUE(compiler.has_diagnostics());
  EXPECT_EQ(compiler.diagnostics().size(), 1);
  EXPECT_EQ(compiler.diagnostics()[0].message, "'main' function must have return type 'int' or 'unit'");
  EXPECT_EQ(compiler.diagnostics()[0].position.start.line, 2);
  EXPECT_EQ(compiler.diagnostics()[0].position.start.column, 9);
}


}  // namespace xylo
