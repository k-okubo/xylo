
#include "xylo/syntax/verifier.h"

#include <gtest/gtest.h>

#include "xylo/syntax/context.h"
#include "xylo/syntax/inferencer.h"
#include "xylo/syntax/lexer.h"
#include "xylo/syntax/parser.h"
#include "xylo/syntax/resolver.h"

namespace xylo {

static_assert(!std::copyable<Verifier>);
static_assert(!std::movable<Verifier>);


static FileASTPtr GetInferredAST(XyloContext* context, const char* source) {
  Lexer lexer(context, source);
  Parser parser(context, &lexer);
  Resolver resolver(context);
  Inferencer inferencer(context);

  auto file_ast = parser.ParseFile();
  assert(!parser.has_diagnostics());

  resolver.VisitFileAST(file_ast.get());
  assert(!resolver.has_diagnostics());

  inferencer.VisitFileAST(file_ast.get());
  assert(!inferencer.has_diagnostics());

  return file_ast;
}


TEST(VerifierTest, SimpleFunction) {
  auto source = R"(
    def main() {
      return 42
    }
  )";

  XyloContext context;
  auto file_ast = GetInferredAST(&context, source);

  Verifier verifier(&context);
  verifier.VisitFileAST(file_ast.get());
  ASSERT_FALSE(verifier.has_diagnostics());
}


TEST(VerifierTest, MissingReturn) {
  auto source = R"(
    def main(): int {
      let x = 10
    }
  )";

  XyloContext context;
  auto file_ast = GetInferredAST(&context, source);

  Verifier verifier(&context);
  verifier.VisitFileAST(file_ast.get());
  ASSERT_TRUE(verifier.has_diagnostics());
  EXPECT_EQ(verifier.diagnostics().size(), 1);
  EXPECT_EQ(verifier.diagnostics()[0].message, "not all paths return a value");
  EXPECT_EQ(verifier.diagnostics()[0].position.start.line, 2);
  EXPECT_EQ(verifier.diagnostics()[0].position.start.column, 13);
}


TEST(VerifierTest, UnreachableReturn) {
  auto source = R"(
    def main(): int {
      return 42
      return 10
    }
  )";

  XyloContext context;
  auto file_ast = GetInferredAST(&context, source);

  Verifier verifier(&context);
  verifier.VisitFileAST(file_ast.get());
  ASSERT_TRUE(verifier.has_diagnostics());
  EXPECT_EQ(verifier.diagnostics().size(), 1);
  EXPECT_EQ(verifier.diagnostics()[0].message, "unreachable statement");
  EXPECT_EQ(verifier.diagnostics()[0].position.start.line, 4);
  EXPECT_EQ(verifier.diagnostics()[0].position.start.column, 7);
}


TEST(VerifierTest, VoidFunctionNoReturn) {
  auto source = R"(
    def main() {
      let x = 10
    }
  )";

  XyloContext context;
  auto file_ast = GetInferredAST(&context, source);

  Verifier verifier(&context);
  verifier.VisitFileAST(file_ast.get());
  ASSERT_FALSE(verifier.has_diagnostics());
}


TEST(VerifierTest, NotEnoughReturns) {
  auto source = R"(
    def main(): int {
      if (true) {
        return 42
      } else {
        let x = 10
      }
    }
  )";

  XyloContext context;
  auto file_ast = GetInferredAST(&context, source);

  Verifier verifier(&context);
  verifier.VisitFileAST(file_ast.get());
  ASSERT_TRUE(verifier.has_diagnostics());
  EXPECT_EQ(verifier.diagnostics().size(), 1);
  EXPECT_EQ(verifier.diagnostics()[0].message, "not all paths return a value");
  EXPECT_EQ(verifier.diagnostics()[0].position.start.line, 2);
  EXPECT_EQ(verifier.diagnostics()[0].position.start.column, 13);
}


}  // namespace xylo
