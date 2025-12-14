
#include "xylo/syntax/inferencer.h"

#include <gtest/gtest.h>

#include "xylo/syntax/context.h"
#include "xylo/syntax/lexer.h"
#include "xylo/syntax/parser.h"
#include "xylo/syntax/resolver.h"

namespace xylo {

static_assert(!std::copyable<Inferencer>);
static_assert(!std::movable<Inferencer>);


static FileASTPtr GetResolvedAST(XyloContext* context, const char* source) {
  Lexer lexer(context, source);
  Parser parser(context, &lexer);
  Resolver resolver(context);

  auto file_ast = parser.ParseFile();
  assert(!parser.has_diagnostics());
  context->root_scope()->Tour();

  resolver.VisitFileAST(file_ast.get());
  assert(!resolver.has_diagnostics());

  return file_ast;
}


TEST(InferencerTest, LiteralAndLet) {
  auto source = R"(
    def main() {
      let a = 42
      let b = a
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 1);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 2);

  auto let_stmt1 = main_statements[0]->As<LetStatement>();
  auto literal_expr = let_stmt1->expr();

  auto let_stmt2 = main_statements[1]->As<LetStatement>();
  auto ident_expr = let_stmt2->expr()->As<IdentifierExpression>();

  EXPECT_EQ(literal_expr->type(), context.int_type());
  EXPECT_EQ(ident_expr->type(), context.int_type());
}


TEST(InferencerTest, LiteralAndVar) {
  auto source = R"(
    def main() {
      var x = 10
      x = x + 0.1
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 1);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 2);

  auto var_stmt = main_statements[0]->As<VarStatement>();
  auto init_expr = var_stmt->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(var_stmt->symbol()->type()->Zonk(&subst, true, &arena), context.float_type());
  EXPECT_EQ(init_expr->type(), context.int_type());
}


TEST(InferencerTest, BindUnitToLet) {
  auto source = R"(
    def main() {
      let a = foo()
    }

    def foo() {
      return
    }
  )";
  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 1);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 1);

  auto let_stmt1 = main_statements[0]->As<LetStatement>();

  TypeArena arena;
  Substitution subst;
  EXPECT_EQ(let_stmt1->symbol()->type()->Zonk(&subst, true, &arena), context.unit_type());

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()), "() -> unit");
}


TEST(InferencerTest, BindUnitToVar) {
  auto source = R"(
    def main() {
      var a = foo()
    }

    def foo() {
      return
    }
  )";
  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 1);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 1);

  auto var_stmt1 = main_statements[0]->As<VarStatement>();

  TypeArena arena;
  Substitution subst;
  EXPECT_EQ(var_stmt1->symbol()->type()->Zonk(&subst, true, &arena), context.unit_type());

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()), "() -> unit");
}


TEST(InferencerTest, CompareSameType) {
  auto source = R"(
    def main() {
      let a = 10
      let b = 10
      return a == b ? 1 : 0
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 1);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 3);

  auto return_stmt = main_statements[2]->As<ReturnStatement>();
  auto ternary_expr = return_stmt->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(ternary_expr->type()->Zonk(&subst, true, &arena), context.int_type());
}


TEST(InferencerTest, CompareDifferentTypes) {
  auto source = R"(
    def main() {
      let a = 10
      let b = 10.5
      return a == b ? 1 : 0
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "operator '==': both sides must have the same type");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 5);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 16);
}


TEST(InferencerTest, ConditionIsNotBool) {
  auto source = R"(
    def main() {
      let a = 10
      if (a) {
        return 1
      } else {
        return 0
      }
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "condition must have boolean type");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 4);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 11);
}


TEST(InferencerTest, CannotAssignToLetVariable) {
  auto source = R"(
    def main() {
      let y = 20
      y = y + 10
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "cannot assign to left operand");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 4);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 9);
}


TEST(InferencerTest, CannotAssignToExpression) {
  auto source = R"(
    def main() {
      let z = 30
      (z + 5) = 100
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "cannot assign to left operand");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 4);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 15);
}


TEST(InferencerTest, CannotAssignToFunction) {
  auto source = R"(
    def main() {
      foo = 50
    }

    def foo() {
      return 42
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "cannot assign to left operand");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 11);
}


TEST(InferencerTest, CannotBindSingleBranch) {
  auto source = R"(
    def main() {
      let x = if (true) 10
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "missing 'else' branch");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 15);
}


TEST(InferencerTest, CannotBindEmptyBranch) {
  auto source = R"(
    def main() {
      let x = if (true) { 10 } else {}
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "block is empty");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 37);
}


TEST(InferencerTest, CannotBindBranchWithoutExpression) {
  auto source = R"(
    def main() {
      let x = if (true) { 10 } else { let y = 20 }
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "block must end with an expression");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 39);
}


TEST(InferencerTest, CannotBindBottomType) {
  auto source = R"(
    def main() {
      let x = if (true) { return } else { return }
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "conditional expression has no value");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 15);
}


TEST(InferencerTest, ConstTypeFunction) {
  auto source = R"(
    def main() {
      let dog = GetDog()
    }

    def GetDog() {
      return new Dog{}
    }

    class Dog {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 3);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 1);

  auto let_stmt = main_statements[0]->As<LetStatement>();
  auto apply_expr = let_stmt->expr();

  auto dog_class_decl = file_ast->declarations()[2]->As<ClassDeclaration>();
  auto dog_type = dog_class_decl->symbol()->type();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr->type()->Zonk(&subst, true, &arena), dog_type);

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()), "() -> Dog");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()), "() -> Dog");
}


TEST(InferencerTest, FuncTypeRepr_ParamTuple) {
  auto source = R"(
    def main() {
      let x = foo(42)
    }

    def foo(x: (int, int)) => x
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "unsupported type in declaration");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 20);
}


TEST(InferencerTest, FuncTypeRepr_ReturnTuple) {
  auto source = R"(
    def main() {
      return 0
    }
    def foo(): () {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "unsupported type in declaration");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 5);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 16);
}


TEST(InferencerTest, FuncTypeRepr_VoidToUnit) {
  auto source = R"(
    def main() {
      do_nothing()
    }
    def do_nothing(): unit {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 2);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 1);

  auto let_stmt = main_statements[0]->As<ExpressionStatement>();
  auto apply_expr = let_stmt->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr->type()->Zonk(&subst, true, &arena), context.unit_type());

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()), "() -> unit");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()), "() -> unit");
}


TEST(InferencerTest, ReturnTypeMismatch_Explicit) {
  auto source = R"(
    def main() {
      let dog = GetDog()
    }

    def GetDog(): Dog {
      return new Cat{}
    }

    class Dog {}
    class Cat {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "incompatible return type");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 7);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 7);
}


TEST(InferencerTest, ReturnTypeMismatch_Inferred) {
  auto source = R"(
    def main() {
      let dog = GetDog()
    }

    def GetDog() {
      if (true) {
        return new Dog{}
      } else {
        return 10
      }
    }

    class Dog {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "incompatible return type");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 10);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 9);
}


TEST(InferencerTest, ReturnTypeMismatch_MissingValue) {
  auto source = R"(
    def main() {
      return 0
    }
    def foo(): int {
      return
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "incompatible return type");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 7);
}


TEST(InferencerTest, AddFunction_Polymorphic) {
  auto source = R"(
    def main() {
      let x = add(3, 5)
      let y = add(2.5, 4.5)
    }

    def add(a, b) => a + b
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 2);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 2);

  auto let_stmt1 = main_statements[0]->As<LetStatement>();
  auto apply_expr1 = let_stmt1->expr();

  auto let_stmt2 = main_statements[1]->As<LetStatement>();
  auto apply_expr2 = let_stmt2->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr1->type()->Zonk(&subst, true, &arena), context.int_type());
  EXPECT_EQ(apply_expr2->type()->Zonk(&subst, true, &arena), context.float_type());

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()),
            "forall A B C. (A, B) -> C where "
            "<Bottom> <: A <: (Numeric & C), <Bottom> <: B <: (Numeric & C), (A | B) <: C <: <Top>");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()),
            "(A, B) -> C where A <: (Numeric & C), B <: (Numeric & C), (A | B) <: C");
}


TEST(InferencerTest, AddFunction_DeclaredTypes) {
  auto source = R"(
    def main() {
      let x = add(3, 5)
      let y = add(2.5, 4.5)
    }

    def add(a: int, b: int) => a + b
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "function call has incompatible argument types");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 4);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 18);
}


TEST(InferencerTest, IdentityFunction_NominalType) {
  auto source = R"(
    def main() {
      let a = id(new Dog{})
      let b = id(new Cat{})
    }

    def id(x) => x

    class Dog {}
    class Cat {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 4);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 2);

  auto let_stmt1 = main_statements[0]->As<LetStatement>();
  auto apply_expr1 = let_stmt1->expr();

  auto let_stmt2 = main_statements[1]->As<LetStatement>();
  auto apply_expr2 = let_stmt2->expr();

  auto dog_class_decl = file_ast->declarations()[2]->As<ClassDeclaration>();
  auto dog_type = dog_class_decl->symbol()->type();
  auto cat_class_decl = file_ast->declarations()[3]->As<ClassDeclaration>();
  auto cat_type = cat_class_decl->symbol()->type();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr1->type()->Zonk(&subst, true, &arena), dog_type);
  EXPECT_EQ(apply_expr2->type()->Zonk(&subst, true, &arena), cat_type);

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()), "forall A. A -> A");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()), "A -> A");
}


TEST(InferencerTest, IdentityFunction_FunctionType) {
  auto source = R"(
    def main() {
      let a = id(add)(3, 5)
    }

    def id(x) => x
    def add(a, b) => a + b
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 3);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 1);

  auto let_stmt1 = main_statements[0]->As<LetStatement>();
  auto apply_expr1 = let_stmt1->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr1->type()->Zonk(&subst, true, &arena), context.int_type());

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()), "forall A. A -> A");
  EXPECT_EQ(vtp(file_ast->declarations()[2]->symbol()->type()),
            "forall A B C. (A, B) -> C where "
            "<Bottom> <: A <: (Numeric & C), <Bottom> <: B <: (Numeric & C), (A | B) <: C <: <Top>");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()), "A -> A");
  EXPECT_EQ(stp(file_ast->declarations()[2]->symbol()->type()),
            "(A, B) -> C where A <: (Numeric & C), B <: (Numeric & C), (A | B) <: C");
}


TEST(InferencerTest, ApplyFunction) {
  auto source = R"(
    def main() {
      let a = Apply(add_one, 5)
    }

    def Apply(f, x) => f(x)
    def add_one(x) => x + 1
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 3);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 1);

  auto let_stmt = main_statements[0]->As<LetStatement>();
  auto apply_expr = let_stmt->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr->type()->Zonk(&subst, true, &arena), context.int_type());

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()),
            "forall A B C D. (A, B) -> C where "
            "<Bottom> <: A = (D => C) <: <Top>, <Bottom> <: B <: D, B <: D <: <Top>");
  EXPECT_EQ(vtp(file_ast->declarations()[2]->symbol()->type()),
            "forall A B. A -> B where <Bottom> <: A <: (Numeric & B), (A | int) <: B <: Comparable");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()), "(A -> B, C) -> B where C <: A");
  EXPECT_EQ(stp(file_ast->declarations()[2]->symbol()->type()),
            "A -> B where A <: (Numeric & B), (A | int) <: B <: Comparable");
}


TEST(InferencerTest, ApplyFunction_WithTypeConstraints1) {
  auto source = R"(
    def main() {
      let a = CondApply(CreateCat, 1)
    }

    def CondApply(f, x) {
      return x == 0 ? new Dog{} : f(x)
    }

    def CreateCat(x) {
      return new Cat{}
    }

    interface Animal {}
    class Dog : Animal {}
    class Cat : Animal {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 6);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 1);

  auto let_stmt = main_statements[0]->As<LetStatement>();
  auto apply_expr = let_stmt->expr();

  auto animal_interface_decl = file_ast->declarations()[3]->As<InterfaceDeclaration>();
  auto animal_type = animal_interface_decl->symbol()->type();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr->type()->Zonk(&subst, true, &arena), animal_type);

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()),
            "forall A B C D E. (A, B) -> C where "
            "<Bottom> <: A = (D => E) <: <Top>, int <: B <: (int & D), (Dog | E) <: C <: Animal, "
            "(int | B) <: D <: Comparable, <Bottom> <: E <: (Animal & C)");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()),
            "(A -> B, int) -> C where int <: A <: Comparable, (Dog | B) <: C <: Animal");
}


TEST(InferencerTest, ApplyFunction_WithTypeConstraints2) {
  auto source = R"(
    def main() {
      let a = CondApply(GetZero, 1)
    }

    def CondApply(f, x) {
      return x == 0 ? new Dog{} : f(x)
    }

    def GetZero(x) {
      return 0
    }

    class Dog {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "function call has incompatible argument types");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 24);
}


TEST(InferencerTest, FunctionParameterMultiUse) {
  auto source = R"(
    def main() {
      let a = double_apply(add_one, 2)
    }

    def double_apply(f, x) {
      return f(x) + f(x)
    }

    def add_one(x) {
      return x + 1
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());
  ASSERT_GE(file_ast->declarations().size(), 3);

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()),
            "forall A B C D. (A, B) -> C where "
            "<Bottom> <: A = (D => C) <: <Top>, <Bottom> <: B <: D, <Bottom> <: C <: Numeric, B <: D <: <Top>");
  EXPECT_EQ(vtp(file_ast->declarations()[2]->symbol()->type()),
            "forall A B. A -> B where <Bottom> <: A <: (Numeric & B), (A | int) <: B <: Comparable");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()), "(A -> B, C) -> B where C <: A, B <: Numeric");
  EXPECT_EQ(stp(file_ast->declarations()[2]->symbol()->type()),
            "A -> B where A <: (Numeric & B), (A | int) <: B <: Comparable");
}


TEST(InferencerTest, CurriedFunction_Simple) {
  auto source = R"(
    def main() {
      let a = CurriedApply(add_one)(5)
    }

    def CurriedApply(f)(x) {
      return f(x)
    }

    def add_one(x) {
      return x + 1
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 3);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 1);

  auto let_stmt = main_statements[0]->As<LetStatement>();
  auto apply_expr = let_stmt->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr->type()->Zonk(&subst, true, &arena), context.int_type());

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()),
            "forall A B C D E F. A -> B where "
            "<Bottom> <: A = (C => D) <: <Top>, <Bottom> <: B = (E => F) <: <Top>, "
            "E <: C <: <Top>, <Bottom> <: D <: F, <Bottom> <: E <: C, D <: F <: <Top>");
  EXPECT_EQ(vtp(file_ast->declarations()[2]->symbol()->type()),
            "forall A B. A -> B where <Bottom> <: A <: (Numeric & B), (A | int) <: B <: Comparable");
  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()), "(A -> B) -> C -> D where C <: A, B <: D");
  EXPECT_EQ(stp(file_ast->declarations()[2]->symbol()->type()),
            "A -> B where A <: (Numeric & B), (A | int) <: B <: Comparable");
}


TEST(InferencerTest, CurriedFunction_InnerFuncNotPolymorphic) {
  auto source = R"(
    def main() {
      let f = CurriedApply(id)
      let a = f(new Foo{})
      let b = f(new Bar{})
    }

    def CurriedApply(f)(x) => f(x)
    def id(x) => x

    class Foo {}
    class Bar {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);

  EXPECT_EQ(inferencer.diagnostics()[0].message, "function call has incompatible argument types");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 5);  // line number of 'let b = f(new Bar{})'
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 16);
}


TEST(InferencerTest, NestedFunction_ReturnsOuterVar) {
  auto source = R"(
    def main() {
      let a = f(new Dog{})
    }

    def f(a) {
      return g()

      def g() {
        return a
      }
    }

    class Dog {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 3);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 1);

  auto let_stmt = main_statements[0]->As<LetStatement>();
  auto apply_expr = let_stmt->expr();

  auto dog_class_decl = file_ast->declarations()[2]->As<ClassDeclaration>();
  auto dog_type = dog_class_decl->symbol()->type();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr->type()->Zonk(&subst, true, &arena), dog_type);

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()), "forall A. A -> A");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()), "A -> A");
}


TEST(InferencerTest, NestedFunction_InnerFuncPolymorphic) {
  auto source = R"(
    def main() {
      let a = Apply(add_one, 5)
      let b = Apply(id, new Dog{})
    }

    def Apply(func, arg) {
      return id_inner(func)(id_inner(arg))

      def id_inner(x) {
        return x
      }
    }

    def add_one(x) => x + 1
    def id(x) => x

    class Dog {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 5);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 2);

  auto let_stmt1 = main_statements[0]->As<LetStatement>();
  auto apply_expr1 = let_stmt1->expr();

  auto let_stmt2 = main_statements[1]->As<LetStatement>();
  auto apply_expr2 = let_stmt2->expr();

  auto dog_class_decl = file_ast->declarations()[4]->As<ClassDeclaration>();
  auto dog_type = dog_class_decl->symbol()->type();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr1->type()->Zonk(&subst, true, &arena), context.int_type());
  EXPECT_EQ(apply_expr2->type()->Zonk(&subst, true, &arena), dog_type);

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()),
            "forall A B C D. (A, B) -> C where <Bottom> <: A = (D => C) <: <Top>, <Bottom> <: B <: D, B <: D <: <Top>");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()), "(A -> B, C) -> B where C <: A");
}


TEST(InferencerTest, RecursiveFunction_NoTypeRepr) {
  auto source = R"(
    def main() {
      let a = factorial(5)
    }

    def factorial(n) {
      return n == 0 ? 1 : n * factorial(n - 1)
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "recursive functions must have an explicit type annotation");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 9);
}


TEST(InferencerTest, RecursiveFunction_WithTypeRepr) {
  auto source = R"(
    def main() {
      let a = factorial(5)
    }

    def factorial(n: int): int {
      return n == 0 ? 1 : n * factorial(n - 1)
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 2);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 1);

  auto let_stmt1 = main_statements[0]->As<LetStatement>();
  auto apply_expr = let_stmt1->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr->type()->Zonk(&subst, true, &arena), context.int_type());

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()), "int -> int");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()), "int -> int");
}


TEST(InferencerTest, InvalidRecursiveFunction) {
  auto source = R"(
    def main() {
      let factorial = fn(f, n) {
        return n == 0 ? 1: f(f, n - 1)
      }
      return factorial(factorial, 5)
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 2);

  EXPECT_EQ(inferencer.diagnostics()[0].message, "value 'f' is not callable");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 4);  // line number of 'return n == 0 ? 1: f(f, n - 1)'
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 28);

  EXPECT_EQ(inferencer.diagnostics()[1].message, "function call has incompatible argument types");
  EXPECT_EQ(inferencer.diagnostics()[1].position.start.line, 6);  // line number of 'return factorial(factorial, 5)'
  EXPECT_EQ(inferencer.diagnostics()[1].position.start.column, 23);
}


TEST(InferencerTest, MutualRecursion_NoTypeRepr) {
  auto source = R"(
    def main() {
      let a = is_even(4)
      let b = is_odd(5)
    }

    def is_even(n) {
      return n == 0 ? true : is_odd(n - 1)
    }

    def is_odd(n) {
      return n == 0 ? false : is_even(n - 1)
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "recursive functions must have an explicit type annotation");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 7);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 9);
}


TEST(InferencerTest, MutualRecursion_WithTypeRepr) {
  auto source = R"(
    def main() {
      let a = is_even(4)
      let b = is_odd(5)
    }

    def is_even(n: int): bool {
      return n == 0 ? true : is_odd(n - 1)
    }

    def is_odd(n: int): bool {
      return n == 0 ? false : is_even(n - 1)
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 3);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 2);

  auto let_stmt1 = main_statements[0]->As<LetStatement>();
  auto apply_expr1 = let_stmt1->expr();

  auto let_stmt2 = main_statements[1]->As<LetStatement>();
  auto apply_expr2 = let_stmt2->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr1->type()->Zonk(&subst, true, &arena), context.bool_type());
  EXPECT_EQ(apply_expr2->type()->Zonk(&subst, true, &arena), context.bool_type());

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()), "int -> bool");
  EXPECT_EQ(vtp(file_ast->declarations()[2]->symbol()->type()), "int -> bool");

  TypePrinter st;
  EXPECT_EQ(st(file_ast->declarations()[1]->symbol()->type()), "int -> bool");
  EXPECT_EQ(st(file_ast->declarations()[2]->symbol()->type()), "int -> bool");
}


TEST(InferencerTest, TypeInferenceByNestedFunc) {
  auto source = R"(
    def main() {
      foo(2)
    }

    def foo(n) {
      bar(1)

      def bar(x) {
        x == 0
        n == x
      }
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[1]->symbol()->type()), "forall A. A -> unit where int <: A <: int");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[1]->symbol()->type()), "int -> unit");
}


TEST(InferencerTest, MergeType_Incompatible_Direct) {
  auto source = R"(
    def main() {
      let x = true ? new Foo{} : new Bar{}
    }

    class Foo {}
    class Bar {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "incompatible types in conditional branches");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 20);
}


TEST(InferencerTest, MergeType_Incompatible_FuncParam) {
  auto source = R"(
    def main() {
      merge(new Foo{}, new Bar{})
    }

    class Foo {}
    class Bar {}

    def merge(a, b) {
      let x = true ? a : b
      return x
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "function call has incompatible argument types");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 12);
}


TEST(InferencerTest, MergeType_IncompatibleButUnused) {
  auto source = R"(
    def main() {
      merge(new Foo{}, new Bar{})
    }

    class Foo {}
    class Bar {}

    def merge(a, b) {
      let x = true ? a : b
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[3]->symbol()->type()), "forall A B. (A, B) -> unit");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[3]->symbol()->type()), "(A, B) -> unit");
}


TEST(InferencerTest, CannotCallNonFunction) {
  auto source = R"(
    def main() {
      let x = 5
      x(10)
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "value 'x' is not callable");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 4);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 7);
}


TEST(InferencerTest, CannotCallExpression) {
  auto source = R"(
    def main() {
      let x = 5
      (x + 1)(10)
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "expression is not callable");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 4);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 14);
}


TEST(InferencerTest, NewObject) {
  auto source = R"(
    class Point { x: int; y: int; }

    def main() {
      let p = new Point{ x: 10, y: 20 }
      let x = p.x
      let y = p.y
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 2);

  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "Point");
  auto point_decl = file_ast->declarations()[0]->As<ClassDeclaration>();
  auto point_type = point_decl->symbol()->type();
  ASSERT_NE(point_type, nullptr);

  ASSERT_EQ(file_ast->declarations()[1]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[1]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 3);

  auto let_stmt = main_statements[0]->As<LetStatement>();
  auto init_expr = let_stmt->expr();

  EXPECT_EQ(init_expr->type(), point_type);
  auto nominal_type = point_type->As<NominalType>();
  ASSERT_EQ(nominal_type->fields().size(), 2);
  EXPECT_EQ(nominal_type->fields()[0]->name()->str().cpp_str(), "x");
  EXPECT_EQ(nominal_type->fields()[1]->name()->str().cpp_str(), "y");
  EXPECT_EQ(nominal_type->fields()[0]->type(), context.int_type());
  EXPECT_EQ(nominal_type->fields()[1]->type(), context.int_type());

  auto let_stmt_x = main_statements[1]->As<LetStatement>();
  auto let_stmt_y = main_statements[2]->As<LetStatement>();
  EXPECT_EQ(let_stmt_x->symbol()->name()->str().cpp_str(), "x");
  EXPECT_EQ(let_stmt_y->symbol()->name()->str().cpp_str(), "y");

  TypeArena arena;
  Substitution subst;
  EXPECT_EQ(let_stmt_x->symbol()->type()->Zonk(&subst, true, &arena), context.int_type());
  EXPECT_EQ(let_stmt_y->symbol()->type()->Zonk(&subst, true, &arena), context.int_type());
}


TEST(InferencerTest, NewObject_MissingField) {
  auto source = R"(
    class Point { x: int; y: int; }

    def main() {
      let p = new Point{ x: 10 }
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "missing field 'y'");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 5);  // line number of 'let p = new Point{ x: 10 }'
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 19);
}


TEST(InferencerTest, NewObject_ExtraField) {
  auto source = R"(
    class Point { x: int; y: int; }

    def main() {
      let p = new Point{ x: 10, y: 20, z: 30 }
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "unknown field 'z'");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 5);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 40);
}


TEST(InferencerTest, NewObject_IncompatibleType) {
  auto source = R"(
    class Point { x: int; y: int; }

    def main() {
      let p = new Point{ x: 10.5, y: 20 }
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "field 'x' has incompatible type");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 5);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 26);
}


TEST(InferencerTest, NewObject_NonClass) {
  auto source = R"(
    def main() {
      let p = new int{}
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "type 'int' cannot be instantiated");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 19);
}


TEST(InferencerTest, SetterFunction) {
  auto source = R"(
    class Foo { a: int }

    def main() {
      let foo = new Foo { a: 5 }
      set_a(foo, 4.5)
      return foo.a < 5 ? 1 : 0
    }

    def set_a(foo, value) {
      foo.a = value
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "function call has incompatible argument types");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 12);
}


TEST(InferencerTest, GetterFunction) {
  auto source = R"(
    class Foo { a: int }

    def main() {
      let foo = new Foo { a: 5 }
      return double_a(foo)
    }

    def double_a(foo) {
      return foo.a
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());
  ASSERT_GE(file_ast->declarations().size(), 2);

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[2]->symbol()->type()), "forall A B. A -> B where <Bottom> <: A <: {a: B}");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[2]->symbol()->type()), "A -> B where A <: {a: B}");
}


TEST(InferencerTest, CannotFindMember_DirectRead) {
  auto source = R"(
    class Foo { a: int }

    def main() {
      let foo = new Foo { a: 5 }
      return foo.b
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "cannot find member 'b' in 'Foo'");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 18);
}


TEST(InferencerTest, CannotFindMember_DirectWrite) {
  auto source = R"(
    class Foo { a: int }

    def main() {
      let foo = new Foo { a: 5 }
      foo.b = 10
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "cannot find member 'b' in 'Foo'");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 11);
}


TEST(InferencerTest, CannotFindMember_FunctionRead) {
  auto source = R"(
    class Foo { a: int }

    def main() {
      let foo = new Foo { a: 5 }
      return get_b(foo)
    }

    def get_b(foo) {
      return foo.b
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  ASSERT_EQ(inferencer.diagnostics().size(), 2);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "cannot find member 'b' in 'Foo'");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 10);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 18);

  EXPECT_EQ(inferencer.diagnostics()[1].message, "function call has incompatible argument types");
  EXPECT_EQ(inferencer.diagnostics()[1].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[1].position.start.column, 19);
}


TEST(InferencerTest, CannotFindMember_FunctionWrite) {
  auto source = R"(
    class Foo { a: int }

    def main() {
      let foo = new Foo { a: 5 }
      set_b(foo, 10)
    }

    def set_b(foo, value) {
      foo.b = value
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  ASSERT_EQ(inferencer.diagnostics().size(), 2);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "cannot find member 'b' in 'Foo'");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 10);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 11);

  EXPECT_EQ(inferencer.diagnostics()[1].message, "function call has incompatible argument types");
  EXPECT_EQ(inferencer.diagnostics()[1].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[1].position.start.column, 12);
}


TEST(InferencerTest, IncompatibleMemberType_Direct) {
  auto source = R"(
    class Foo { a: int }

    def main() {
      let foo = new Foo { a: 5 }
      foo.a = 0.0
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "operator '=': operand types are incompatible");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 13);
}


TEST(InferencerTest, IncompatibleMemberType_Function) {
  auto source = R"(
    class Foo { a: int }

    def main() {
      let foo = new Foo { a: 5 }
      reset_a(foo)
    }

    def reset_a(foo) {
      foo.a = 0.0
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  ASSERT_EQ(inferencer.diagnostics().size(), 2);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "member 'a' has incompatible type");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 10);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 11);

  EXPECT_EQ(inferencer.diagnostics()[1].message, "function call has incompatible argument types");
  EXPECT_EQ(inferencer.diagnostics()[1].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[1].position.start.column, 14);
}


TEST(InferencerTest, CannotAssignToMethod_Direct) {
  auto source = R"(
    class Foo {
      def a() => 10
    }

    def main() {
      let foo = new Foo {}
      foo.a = 0
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "cannot assign to left operand");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 8);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 13);
}


TEST(InferencerTest, CannotAssignToMethod_Function) {
  auto source = R"(
    class Foo {
      def a() => 10
    }

    def main() {
      let foo = new Foo {}
      reset_a(foo)
    }

    def reset_a(foo) {
      foo.a = 0
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  ASSERT_EQ(inferencer.diagnostics().size(), 2);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "member 'a' has incompatible type");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 12);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 11);

  EXPECT_EQ(inferencer.diagnostics()[1].message, "function call has incompatible argument types");
  EXPECT_EQ(inferencer.diagnostics()[1].position.start.line, 8);
  EXPECT_EQ(inferencer.diagnostics()[1].position.start.column, 14);
}


TEST(InferencerTest, MethodConstraintInNestedFunc) {
  auto source = R"(
    class Foo {
      def foo() {
        return 42
      }
    }
    class Dummy {
      def foo() {
        return 0
      }
    }

    def main() {
      return outer(new Dummy{})
    }

    def outer(v1) {
      return inner(new Foo{})

      def inner(v2) {
        var a = true ? v1 : v2
        return a.foo()
      }
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "function call has incompatible argument types");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 14);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 19);
}


TEST(InferencerTest, MultiReadFieldFunction) {
  auto source = R"(
    class Foo { a: int }

    def main() {
      let foo = new Foo { a: 5 }
      return double_a(foo)
    }

    def double_a(foo: Foo) {
      return foo.a + foo.a
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());
  ASSERT_GE(file_ast->declarations().size(), 2);

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(file_ast->declarations()[2]->symbol()->type()), "Foo -> int");

  TypePrinter stp;
  EXPECT_EQ(stp(file_ast->declarations()[2]->symbol()->type()), "Foo -> int");
}


TEST(InferencerTest, GetterMethod) {
  auto source = R"(
    class Foo {
      a: float
      def get_a() => a
    }

    def main() {
      let foo = new Foo { a: 10.5 }
      let a = foo.get_a()
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 2);
  ASSERT_EQ(file_ast->declarations()[1]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[1]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 2);

  auto let_stmt = main_statements[1]->As<LetStatement>();
  auto let_expr = let_stmt->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(let_expr->type()->Zonk(&subst, true, &arena), context.float_type());
}


TEST(InferencerTest, SetterMethod) {
  auto source = R"(
    class Foo {
      a: int
      def set_a(value) { a = value }
    }

    def main() {
      let foo = new Foo { a: 10 }
      foo.set_a(20.5)
      return foo.a < 15 ? 1 : 0
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "function call has incompatible argument types");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 9);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 16);
}


TEST(InferencerTest, RecursiveMethod_NoTypeRepr) {
  auto source = R"(
    class Factorial {
      def factorial(n) {
        return n == 0 ? 1 : n * this.factorial(n - 1)
      }
    }

    def main() {
      let f = new Factorial{}
      let a = f.factorial(5)
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);
  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "recursive functions must have an explicit type annotation");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 11);
}


TEST(InferencerTest, RecursiveMethod_WithTypeRepr) {
  auto source = R"(
    class Factorial {
      def factorial(n: int): int {
        return n == 0 ? 1 : n * this.factorial(n - 1)
      }
    }

    def main() {
      let f = new Factorial{}
      let a = f.factorial(5)
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 2);
  ASSERT_EQ(file_ast->declarations()[1]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[1]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 2);

  auto let_stmt1 = main_statements[1]->As<LetStatement>();
  auto apply_expr = let_stmt1->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr->type()->Zonk(&subst, true, &arena), context.int_type());

  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "Factorial");
  auto class_decl = file_ast->declarations()[0]->As<ClassDeclaration>();
  auto factorial_method = class_decl->declarations()[0]->As<FunctionDeclaration>();

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(factorial_method->symbol()->type()), "int => int");

  TypePrinter stp;
  EXPECT_EQ(stp(factorial_method->symbol()->type()), "int -> int");
}


TEST(InferencerTest, MutualRecursionMethod_NoTypeRepr) {
  auto source = R"(
    class EvenOdd {
      def is_even(n) {
        return n == 0 ? true : this.is_odd(n - 1)
      }

      def is_odd(n) {
        return n == 0 ? false : this.is_even(n - 1)
      }
    }

    def main() {
      let eo = new EvenOdd{}
      let a = eo.is_even(4)
      let b = eo.is_odd(5)
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "recursive functions must have an explicit type annotation");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 11);
}


TEST(InferencerTest, MutualRecursionMethod_WithTypeRepr) {
  auto source = R"(
    class EvenOdd {
      def is_even(n: int): bool {
        return n == 0 ? true : this.is_odd(n - 1)
      }

      def is_odd(n: int): bool {
        return n == 0 ? false : this.is_even(n - 1)
      }
    }

    def main() {
      let eo = new EvenOdd{}
      let a = eo.is_even(4)
      let b = eo.is_odd(5)
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 2);
  ASSERT_EQ(file_ast->declarations()[1]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[1]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 3);

  auto let_stmt1 = main_statements[1]->As<LetStatement>();
  auto apply_expr1 = let_stmt1->expr();

  auto let_stmt2 = main_statements[2]->As<LetStatement>();
  auto apply_expr2 = let_stmt2->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr1->type()->Zonk(&subst, true, &arena), context.bool_type());
  EXPECT_EQ(apply_expr2->type()->Zonk(&subst, true, &arena), context.bool_type());

  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "EvenOdd");
  auto class_decl = file_ast->declarations()[0]->As<ClassDeclaration>();
  auto is_even_method = class_decl->declarations()[0]->As<FunctionDeclaration>();
  auto is_odd_method = class_decl->declarations()[1]->As<FunctionDeclaration>();

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(is_even_method->symbol()->type()), "int => bool");
  EXPECT_EQ(vtp(is_odd_method->symbol()->type()), "int => bool");

  TypePrinter stp;
  EXPECT_EQ(stp(is_even_method->symbol()->type()), "int -> bool");
  EXPECT_EQ(stp(is_odd_method->symbol()->type()), "int -> bool");
}


TEST(InferencerTest, MutualRecursionMethod_WithFunc) {
  auto source = R"(
    class EvenOdd {
      def is_even(n: int): bool {
        return n == 0 ? true : is_odd_helper(this, n - 1)
      }

      def is_odd(n: int): bool {
        return n == 0 ? false : is_even_helper(this, n - 1)
      }
    }

    def is_even_helper(eo: EvenOdd, n: int): bool {
      return eo.is_even(n)
    }

    def is_odd_helper(eo: EvenOdd, n: int): bool {
      return eo.is_odd(n)
    }

    def main() {
      let eo = new EvenOdd{}
      let a = eo.is_even(4)
      let b = eo.is_odd(5)
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 4);
  ASSERT_EQ(file_ast->declarations()[3]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[3]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 3);

  auto let_stmt1 = main_statements[1]->As<LetStatement>();
  auto apply_expr1 = let_stmt1->expr();

  auto let_stmt2 = main_statements[2]->As<LetStatement>();
  auto apply_expr2 = let_stmt2->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(apply_expr1->type()->Zonk(&subst, true, &arena), context.bool_type());
  EXPECT_EQ(apply_expr2->type()->Zonk(&subst, true, &arena), context.bool_type());
}


TEST(InferencerTest, CircularReferenceInClass) {
  auto source = R"(
    class Parent {
      child: Child
    }
    class Child {
      parent: Parent
    }

    def main() {
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 3);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "Parent");
  auto parent_decl = file_ast->declarations()[0]->As<ClassDeclaration>();
  auto parent_type = parent_decl->symbol()->type()->As<NominalType>();
  ASSERT_EQ(parent_type->fields().size(), 1);
  EXPECT_EQ(parent_type->fields()[0]->name()->str().cpp_str(), "child");
  EXPECT_EQ(parent_type->fields()[0]->type(), file_ast->declarations()[1]->symbol()->type());

  ASSERT_EQ(file_ast->declarations()[1]->symbol()->name()->str().cpp_str(), "Child");
  auto child_decl = file_ast->declarations()[1]->As<ClassDeclaration>();
  auto child_type = child_decl->symbol()->type()->As<NominalType>();
  ASSERT_EQ(child_type->fields().size(), 1);
  EXPECT_EQ(child_type->fields()[0]->name()->str().cpp_str(), "parent");
  EXPECT_EQ(child_type->fields()[0]->type(), file_ast->declarations()[0]->symbol()->type());
}


TEST(InferencerTest, NestedClassInPolymorphicFunction1) {
  auto source = R"(
    def main() {
      let a = outer(10).get()
      let b = outer(3.14).get()
    }

    def outer(a) {
      return new Inner{}

      class Inner {
        def get() => a
      }
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 2);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 2);

  auto let_stmt1 = main_statements[0]->As<LetStatement>();
  auto let_expr1 = let_stmt1->expr();

  auto let_stmt2 = main_statements[1]->As<LetStatement>();
  auto let_expr2 = let_stmt2->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(let_expr1->type()->Zonk(&subst, true, &arena), context.int_type());
  EXPECT_EQ(let_expr2->type()->Zonk(&subst, true, &arena), context.float_type());
}


TEST(InferencerTest, NestedClassInPolymorphicFunction2) {
  auto source = R"(
    def main() {
      let inst = bridge()
      let a = inst.get(new Dog{})
      let b = inst.get(new Cat{})
    }

    def bridge() {
      return outer(true)
    }

    def outer(c) {
      return middle(new Dog{})

      def middle(a) {
        return new Inner{}

        class Inner {
          def get(b) => c ? a : b
        }
      }
    }

    interface Animal {}
    class Dog : Animal {}
    class Cat : Animal {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 6);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 3);

  auto let_stmt1 = main_statements[0]->As<LetStatement>();
  auto let_expr1 = let_stmt1->expr();

  auto let_stmt2 = main_statements[1]->As<LetStatement>();
  auto let_expr2 = let_stmt2->expr();

  auto let_stmt3 = main_statements[2]->As<LetStatement>();
  auto let_expr3 = let_stmt3->expr();

  auto animal_interface_decl = file_ast->declarations()[3]->As<InterfaceDeclaration>();
  auto animal_type = animal_interface_decl->symbol()->type();
  auto dog_class_decl = file_ast->declarations()[4]->As<ClassDeclaration>();
  auto dog_type = dog_class_decl->symbol()->type();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(let_expr2->type()->Zonk(&subst, true, &arena), dog_type);
  EXPECT_EQ(let_expr3->type()->Zonk(&subst, true, &arena), animal_type);

  auto get_method_type = let_expr1->type()->Zonk(&subst, true, &arena)->As<NominalType>()->methods()[0]->type();

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(get_method_type),
            "[A := bool] [B := Dog] forall C D. C => D where "
            "<Bottom> <: A <: bool, <Bottom> <: C <: D, (B | C) <: D <: <Top>");

  TypePrinter stp;
  EXPECT_EQ(stp(get_method_type), "A -> B where (Dog | A) <: B");
}


TEST(InferencerTest, NestedClassInPolymorphicFunction3) {
  auto source = R"(
    def main() {
      let inst = outer(new Dog{}, true)
      let a = inst.get(new Dog{})
      let b = inst.get(new Cat{})
    }

    def outer(ax, c) {
      return middle(ax)

      def middle(a) {
        return new Inner{}

        class Inner {
          def get(b) => c ? a : b
        }
      }
    }

    interface Animal {}
    class Dog : Animal {}
    class Cat : Animal {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 5);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 3);

  auto let_stmt1 = main_statements[0]->As<LetStatement>();
  auto let_expr1 = let_stmt1->expr();

  auto let_stmt2 = main_statements[1]->As<LetStatement>();
  auto let_expr2 = let_stmt2->expr();

  auto let_stmt3 = main_statements[2]->As<LetStatement>();
  auto let_expr3 = let_stmt3->expr();

  auto animal_interface_decl = file_ast->declarations()[2]->As<InterfaceDeclaration>();
  auto animal_type = animal_interface_decl->symbol()->type();
  auto dog_class_decl = file_ast->declarations()[3]->As<ClassDeclaration>();
  auto dog_type = dog_class_decl->symbol()->type();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(let_expr2->type()->Zonk(&subst, true, &arena), dog_type);
  EXPECT_EQ(let_expr3->type()->Zonk(&subst, true, &arena), animal_type);

  auto get_method_type = let_expr1->type()->Zonk(&subst, true, &arena)->As<NominalType>()->methods()[0]->type();

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(
      vtp(get_method_type),
      "[A := B', C := D'] [E := C] forall F G. F => G where "
      "<Bottom> <: A <: bool, bool <: B' <: bool, Dog <: D' <: Animal, <Bottom> <: F <: G, (E | F) <: G <: <Top>");

  TypePrinter stp;
  EXPECT_EQ(stp(get_method_type), "A -> B where (C | A) <: B, Dog <: C <: Animal");
}


TEST(InferencerTest, NestedClassInPolymorphicFunction4) {
  auto source = R"(
    def main() {
      let inst = outer(new Dog{}, new Cat{})
      let a = inst.get()
    }

    def outer(a, b) {
      return middle(true ? a : b)

      def middle(x) {
        return new Inner{}

        class Inner {
          def get() => x
        }
      }
    }

    interface Animal {}
    class Dog : Animal {}
    class Cat : Animal {}
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 5);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 2);

  auto let_stmt1 = main_statements[0]->As<LetStatement>();
  auto let_expr1 = let_stmt1->expr();

  auto let_stmt2 = main_statements[1]->As<LetStatement>();
  auto let_expr2 = let_stmt2->expr();

  auto animal_interface_decl = file_ast->declarations()[2]->As<InterfaceDeclaration>();
  auto animal_type = animal_interface_decl->symbol()->type();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(let_expr2->type()->Zonk(&subst, true, &arena), animal_type);

  auto get_method_type = let_expr1->type()->Zonk(&subst, true, &arena)->As<NominalType>()->methods()[0]->type();

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(get_method_type),
            "() => A' where (B' | C' | Dog | Cat | D') <: A' <: (Animal & D'), "
            "Dog <: B' <: (A' & Animal), Cat <: C' <: (A' & Animal), "
            "(Dog | Cat | A') <: D' <: (Animal & A' & E'), (Dog | Cat | D') <: E' <: Animal");

  TypePrinter stp;
  EXPECT_EQ(stp(get_method_type), "() -> A where (B | C) <: A <: Animal, Dog <: B, Cat <: C");
}


TEST(InferencerTest, Embedding_Basic) {
  auto source = R"(
    def main() {
      let foo = new Foo{
          a: 10,
          Bar: {
            b: 3.14
          }
        }

      let x = foo.a + foo.b
    }

    class Foo {
      a: int
      embed Bar
    }
    class Bar {
      b: float
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 3);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 2);

  auto let_stmt = main_statements[1]->As<LetStatement>();
  auto let_expr = let_stmt->expr();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(let_expr->type()->Zonk(&subst, true, &arena), context.float_type());
}


TEST(InferencerTest, Embedding_AmbiguousField) {
  auto source = R"(
    def main() {
      let foo = new Foo{
          a: 10,
          Bar: {
            b: 3.14
          },
          Baz: {
            b: 2.71
          }
        }

      let x = foo.b
    }

    class Foo {
      a: int
      embed Bar
      embed Baz
    }
    class Bar {
      b: float
    }
    class Baz {
      b: float
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  ASSERT_EQ(inferencer.diagnostics().size(), 2);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "member 'b' is ambiguous");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 13);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 19);

  EXPECT_EQ(inferencer.diagnostics()[1].message, "candidates are from embedded classes 'Bar' and 'Baz'");
  EXPECT_EQ(inferencer.diagnostics()[1].severity, DiagnosticSeverity::kNote);
}


TEST(InferencerTest, Embedding_Cyclic) {
  auto source = R"(
    class A {
      embed B
    }
    class B {
      embed A
    }

    def main() {
      let a = new A{}
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 2);

  EXPECT_EQ(inferencer.diagnostics()[0].message, "cyclic embedding detected");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 13);

  EXPECT_EQ(inferencer.diagnostics()[1].message, "missing field 'B'");
  EXPECT_EQ(inferencer.diagnostics()[1].position.start.line, 10);
  EXPECT_EQ(inferencer.diagnostics()[1].position.start.column, 19);
}


TEST(InferencerTest, Embedding_MissingInitializer) {
  auto source = R"(
    def main() {
      let foo = new Foo{}
    }

    class Foo {
      embed Bar
    }
    class Bar {
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "missing field 'Bar'");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 21);
}


TEST(InferencerTest, Embedding_NonClass) {
  auto source = R"(
    class Foo {
      embed int
    }

    def main() {
      let foo = new Foo{}
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "type 'int' cannot be embedded");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 13);
}


TEST(InferencerTest, Interface_Basic) {
  auto source = R"(
    def main() {
      let foo = select(true)
      return foo.get_value()
    }

    def select(cond) {
      return cond ? new Foo{ value: 42 } : new Bar{}
    }

    interface ValueContainer {
      def get_value(): int
    }

    class Foo : ValueContainer {
      value: int
      def get_value() => value
    }

    class Bar : ValueContainer {
      def get_value() => 0
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_FALSE(inferencer.has_diagnostics());

  ASSERT_GE(file_ast->declarations().size(), 2);
  ASSERT_EQ(file_ast->declarations()[0]->symbol()->name()->str().cpp_str(), "main");
  auto main_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto& main_statements = main_decl->func()->body()->statements();
  ASSERT_GE(main_statements.size(), 2);

  auto let_stmt = main_statements[0]->As<LetStatement>();
  auto let_expr = let_stmt->expr();

  auto return_stmt = main_statements[1]->As<ReturnStatement>();
  auto return_expr = return_stmt->expr();

  ASSERT_EQ(file_ast->declarations()[2]->symbol()->name()->str().cpp_str(), "ValueContainer");
  auto interface_decl = file_ast->declarations()[2]->As<InterfaceDeclaration>();
  auto interface_type = interface_decl->symbol()->type()->As<NominalType>();

  Substitution subst;
  TypeArena arena;
  EXPECT_EQ(let_expr->type()->Zonk(&subst, true, &arena), interface_type);
  EXPECT_EQ(return_expr->type()->Zonk(&subst, true, &arena), context.int_type());
}


TEST(InferencerTest, Interface_MissingTypeAnnotation) {
  auto source = R"(
    interface Printable {
      def print()
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "interface methods must have explicit type annotations");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 3);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 11);
}


TEST(InferencerTest, Interface_MissingImplementation) {
  auto source = R"(
    interface Printable {
      def print(): unit
    }

    class Circle : Printable {
      radius: float
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "method 'print' from interface 'Printable' is not implemented");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 11);
}


TEST(InferencerTest, Interface_IncompatibleImplementation) {
  auto source = R"(
    interface Printable {
      def print(): unit
    }

    class Square : Printable {
      side: float
      def print() => 42
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "method 'print' has incompatible type for interface 'Printable'");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 8);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 11);
}


TEST(InferencerTest, Interface_Cyclic) {
  auto source = R"(
    interface A : B {
      def foo(): int
    }

    interface B : A {
      def bar(): int
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "cyclic inheritance detected");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 6);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 19);
}


TEST(InferencerTest, Interface_AmbiguousMethod) {
  auto source = R"(
    interface A {
      def get(): int
    }

    interface B {
      def get(): int
    }

    class Foo : A, B {
      def get() => 42
    }

    class Bar : A, B {
      def get() => 3
    }

    def main() {
      let f = true ? new Foo{} : new Bar{}
      return f.get()
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  ASSERT_EQ(inferencer.diagnostics().size(), 2);

  EXPECT_EQ(inferencer.diagnostics()[0].message, "method 'get' is ambiguous");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 20);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 16);

  EXPECT_EQ(inferencer.diagnostics()[1].message, "candidates are from interfaces 'A' and 'B'");
  EXPECT_EQ(inferencer.diagnostics()[1].severity, DiagnosticSeverity::kNote);
}


TEST(InferencerTest, Inherit_NonInterface) {
  auto source = R"(
    class Foo : int {
    }
  )";

  XyloContext context;
  auto file_ast = GetResolvedAST(&context, source);

  Inferencer inferencer(&context);
  inferencer.VisitFileAST(file_ast.get());
  ASSERT_TRUE(inferencer.has_diagnostics());
  EXPECT_EQ(inferencer.diagnostics().size(), 1);
  EXPECT_EQ(inferencer.diagnostics()[0].message, "type 'int' cannot be inherited from");
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.line, 2);
  EXPECT_EQ(inferencer.diagnostics()[0].position.start.column, 17);
}


}  // namespace xylo
