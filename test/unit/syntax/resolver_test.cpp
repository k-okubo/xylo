
#include "xylo/syntax/resolver.h"

#include <gtest/gtest.h>

#include "xylo/syntax/lexer.h"
#include "xylo/syntax/parser.h"

namespace xylo {

static_assert(!std::copyable<Resolver>);
static_assert(!std::movable<Resolver>);
static_assert(!std::copyable<NameTable>);
static_assert(!std::movable<NameTable>);


TEST(ResolverTest, VisitFileAST_Basic) {
  auto source = "def foo(x) { return x }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_FALSE(resolver.has_diagnostics());

  auto& declarations = file_ast->declarations();
  ASSERT_EQ(declarations.size(), 1u);

  auto func_decl = declarations[0]->As<FunctionDeclaration>();
  auto func_expr = func_decl->func();

  ASSERT_EQ(func_expr->params().size(), 1u);
  auto param_symbol = func_expr->params()[0]->symbol();

  auto& statements = func_expr->body()->statements();
  ASSERT_EQ(statements.size(), 1u);

  auto return_stmt = statements[0]->As<ReturnStatement>();
  auto return_expr = return_stmt->expr();

  auto ident_expr = return_expr->As<IdentifierExpression>();
  EXPECT_EQ(ident_expr->symbol(), param_symbol);
}


TEST(ResolverTest, VisitFileAST_UndefinedIdentifier) {
  auto source = "def foo() { return x }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_TRUE(resolver.has_diagnostics());
  EXPECT_EQ(resolver.diagnostics().size(), 1);
  EXPECT_EQ(resolver.diagnostics()[0].message, "undeclared identifier 'x'");
  EXPECT_EQ(resolver.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(resolver.diagnostics()[0].position.start.column, 20);
}


TEST(ResolverTest, VisitFileAST_DuplicateFunction) {
  auto source = "def foo(x) { return x }\ndef foo(y) { return y }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_TRUE(resolver.has_diagnostics());
  ASSERT_EQ(resolver.diagnostics().size(), 2);
  EXPECT_EQ(resolver.diagnostics()[0].message, "redefinition of 'foo'");
  EXPECT_EQ(resolver.diagnostics()[0].position.start.line, 2);
  EXPECT_EQ(resolver.diagnostics()[0].position.start.column, 5);

  EXPECT_EQ(resolver.diagnostics()[1].severity, DiagnosticSeverity::kNote);
  EXPECT_EQ(resolver.diagnostics()[1].message, "previous declaration is here");
  EXPECT_EQ(resolver.diagnostics()[1].position.start.line, 1);
  EXPECT_EQ(resolver.diagnostics()[1].position.start.column, 5);
}


TEST(ResolverTest, VisitFileAST_DuplicateParameter) {
  auto source = "def foo(x, y, x) { return x }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_TRUE(resolver.has_diagnostics());
  ASSERT_EQ(resolver.diagnostics().size(), 2);
  EXPECT_EQ(resolver.diagnostics()[0].message, "redefinition of 'x'");
  EXPECT_EQ(resolver.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(resolver.diagnostics()[0].position.start.column, 15);

  EXPECT_EQ(resolver.diagnostics()[1].message, "previous declaration is here");
  EXPECT_EQ(resolver.diagnostics()[1].position.start.line, 1);
  EXPECT_EQ(resolver.diagnostics()[1].position.start.column, 9);
}


TEST(ResolverTest, VisitFileAST_DuplicateVariable) {
  auto source = "def foo(x, y) { let a = x; let a = y; return a }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_TRUE(resolver.has_diagnostics());
  ASSERT_EQ(resolver.diagnostics().size(), 2);
  EXPECT_EQ(resolver.diagnostics()[0].message, "redefinition of 'a'");
  EXPECT_EQ(resolver.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(resolver.diagnostics()[0].position.start.column, 32);

  EXPECT_EQ(resolver.diagnostics()[1].severity, DiagnosticSeverity::kNote);
  EXPECT_EQ(resolver.diagnostics()[1].message, "previous declaration is here");
  EXPECT_EQ(resolver.diagnostics()[1].position.start.line, 1);
  EXPECT_EQ(resolver.diagnostics()[1].position.start.column, 21);
}


TEST(ResolverTest, VisitFileAST_NestedBlockScopes) {
  auto source = "def foo(x) { let a = x; { let b = a; { let c = b; return c } } }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_FALSE(resolver.has_diagnostics());
}


TEST(ResolverTest, VisitFileAST_NestedBlockScopes_DupVar) {
  auto source = "def foo(x) { let a = x; { let a = x; return a } }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_FALSE(resolver.has_diagnostics());
}


TEST(ResolverTest, VisitFileAST_NestedBlockScopes_WrongReference) {
  auto source = "def foo(x) { let a = x; { let b = a }; let c = b }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_TRUE(resolver.has_diagnostics());
  EXPECT_EQ(resolver.diagnostics().size(), 1);
  EXPECT_EQ(resolver.diagnostics()[0].message, "undeclared identifier 'b'");
  EXPECT_EQ(resolver.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(resolver.diagnostics()[0].position.start.column, 48);
}


TEST(ResolverTest, VisitFileAST_ParamCaptureClosure) {
  auto source = R"(
    def foo(x) {
      def addX(a) { return a + x }
      def inc(a) { return a + 1 }
      let b = 10
      return addX(inc(b))
    }
  )";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_FALSE(resolver.has_diagnostics());

  auto& declarations = file_ast->declarations();
  ASSERT_EQ(declarations.size(), 1u);

  auto foo_decl = declarations[0]->As<FunctionDeclaration>();
  auto foo_func = foo_decl->func();

  ASSERT_EQ(foo_func->body()->declarations().size(), 2u);
  auto addx_decl = foo_func->body()->declarations()[0]->As<FunctionDeclaration>();
  auto addx_func = addx_decl->func();
  EXPECT_TRUE(addx_func->is_closure());

  auto inc_decl = foo_func->body()->declarations()[1]->As<FunctionDeclaration>();
  auto inc_func = inc_decl->func();
  EXPECT_FALSE(inc_func->is_closure());

  ASSERT_EQ(foo_func->body()->statements().size(), 2u);
  auto let_b_stmt = foo_func->body()->statements()[0]->As<LetStatement>();
  auto let_b_symbol = let_b_stmt->symbol();
  EXPECT_FALSE(let_b_symbol->is_captured());

  ASSERT_EQ(foo_func->params().size(), 1u);
  auto param_x_symbol = foo_func->params()[0]->symbol();
  EXPECT_TRUE(param_x_symbol->is_captured());
  EXPECT_EQ(param_x_symbol->captured_index(), 0);

  ASSERT_EQ(foo_func->captured_symbols().size(), 1u);
  EXPECT_EQ(foo_func->captured_symbols()[0], param_x_symbol);
}


TEST(ResolverTest, VisitFileAST_FuncCaptureClosure) {
  auto source = R"(
    def foo(x) {
      return nested_get_x()

      def nested_get_x() {
        return get_x()
      }

      def get_x() {
        return x
      }
    }
  )";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_FALSE(resolver.has_diagnostics());

  auto& declarations = file_ast->declarations();
  ASSERT_EQ(declarations.size(), 1u);

  auto foo_decl = declarations[0]->As<FunctionDeclaration>();
  auto foo_func = foo_decl->func();

  ASSERT_EQ(foo_func->body()->declarations().size(), 2u);
  auto nested_get_x_decl = foo_func->body()->declarations()[0]->As<FunctionDeclaration>();
  auto nested_get_x_func = nested_get_x_decl->func();
  EXPECT_TRUE(nested_get_x_func->is_closure());

  auto get_x_decl = foo_func->body()->declarations()[1]->As<FunctionDeclaration>();
  auto get_x_func = get_x_decl->func();
  EXPECT_TRUE(get_x_func->is_closure());

  EXPECT_FALSE(nested_get_x_decl->symbol()->is_captured());
  EXPECT_TRUE(get_x_decl->symbol()->is_captured());

  EXPECT_TRUE(foo_func->params()[0]->symbol()->is_captured());
  EXPECT_EQ(foo_func->captured_symbols().size(), 1u);
}


TEST(ResolverTest, VisitFileAST_DeepClosure) {
  auto source = R"(
    def outer(a) {
      return middle(20)

      def middle() {
        return inner(30)

        def inner(b) {
          return a + b
        }
      }
    }
  )";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_FALSE(resolver.has_diagnostics());

  ASSERT_EQ(file_ast->declarations().size(), 1u);
  auto outer_decl = file_ast->declarations()[0]->As<FunctionDeclaration>();
  auto outer_func = outer_decl->func();
  EXPECT_FALSE(outer_func->is_closure());
  EXPECT_TRUE(outer_func->has_closure());
  EXPECT_EQ(outer_func->captured_symbols().size(), 1u);

  ASSERT_EQ(outer_func->body()->declarations().size(), 1u);
  auto middle_decl = outer_func->body()->declarations()[0]->As<FunctionDeclaration>();
  auto middle_func = middle_decl->func();
  EXPECT_TRUE(middle_func->is_closure());
  EXPECT_TRUE(middle_func->has_closure());
  EXPECT_EQ(middle_func->captured_symbols().size(), 0u);

  ASSERT_EQ(middle_func->body()->declarations().size(), 1u);
  auto inner_decl = middle_func->body()->declarations()[0]->As<FunctionDeclaration>();
  auto inner_func = inner_decl->func();
  EXPECT_TRUE(inner_func->is_closure());
  EXPECT_FALSE(inner_func->has_closure());
  EXPECT_EQ(inner_func->captured_symbols().size(), 0u);
}


TEST(ResolverTest, VisitFileAST_NewExpression) {
  auto source = "class Foo { }\ndef create_foo() { return new Foo{} }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_FALSE(resolver.has_diagnostics());

  auto& declarations = file_ast->declarations();
  ASSERT_EQ(declarations.size(), 2u);

  auto class_decl = declarations[0]->As<ClassDeclaration>();
  auto class_symbol = class_decl->symbol();

  auto func_decl = declarations[1]->As<FunctionDeclaration>();
  auto func_expr = func_decl->func();

  auto& statements = func_expr->body()->statements();
  ASSERT_EQ(statements.size(), 1u);

  auto return_stmt = statements[0]->As<ReturnStatement>();
  auto return_expr = return_stmt->expr();

  auto new_expr = return_expr->As<NewExpression>();
  EXPECT_EQ(new_expr->class_symbol(), class_symbol);
}


TEST(ResolverTest, VisitFileAST_NewExpression_UndefinedClass) {
  auto source = "def create_foo() { return new Foo{} }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_TRUE(resolver.has_diagnostics());
  EXPECT_EQ(resolver.diagnostics().size(), 1);
  EXPECT_EQ(resolver.diagnostics()[0].message, "undeclared identifier 'Foo'");
  EXPECT_EQ(resolver.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(resolver.diagnostics()[0].position.start.column, 31);
}


TEST(ResolverTest, VisitFileAST_NewExpression_DuplicateInitializer) {
  auto source = "class Foo { a : int; b : int }\ndef create_foo() { return new Foo{ a: 10, a: 20 } }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_TRUE(resolver.has_diagnostics());
  EXPECT_EQ(resolver.diagnostics().size(), 1);
  EXPECT_EQ(resolver.diagnostics()[0].message, "duplicate field name 'a'");
  EXPECT_EQ(resolver.diagnostics()[0].position.start.line, 2);
  EXPECT_EQ(resolver.diagnostics()[0].position.start.column, 43);
}


TEST(ResolverTest, VisitFileAST_NewExpression_NonClass) {
  auto source = "def Foo() { }\ndef create_foo() { return new Foo{} }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_TRUE(resolver.has_diagnostics());
  EXPECT_EQ(resolver.diagnostics().size(), 1);
  EXPECT_EQ(resolver.diagnostics()[0].message, "value 'Foo' cannot be used as a type");
  EXPECT_EQ(resolver.diagnostics()[0].position.start.line, 2);
  EXPECT_EQ(resolver.diagnostics()[0].position.start.column, 31);
}


TEST(ResolverTest, VisitFileAST_DuplicateField) {
  auto source = "class Person { name: string; age: int; name: string }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_TRUE(resolver.has_diagnostics());
  ASSERT_EQ(resolver.diagnostics().size(), 2);
  EXPECT_EQ(resolver.diagnostics()[0].message, "redefinition of 'name'");
  EXPECT_EQ(resolver.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(resolver.diagnostics()[0].position.start.column, 40);

  EXPECT_EQ(resolver.diagnostics()[1].severity, DiagnosticSeverity::kNote);
  EXPECT_EQ(resolver.diagnostics()[1].message, "previous declaration is here");
  EXPECT_EQ(resolver.diagnostics()[1].position.start.line, 1);
  EXPECT_EQ(resolver.diagnostics()[1].position.start.column, 16);
}


TEST(ResolverTest, VisitFileAST_DuplicateMember) {
  auto source = "class Person { name: string; def name() { } }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_TRUE(resolver.has_diagnostics());
  ASSERT_EQ(resolver.diagnostics().size(), 2);
  EXPECT_EQ(resolver.diagnostics()[0].message, "redefinition of 'name'");
  EXPECT_EQ(resolver.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(resolver.diagnostics()[0].position.start.column, 34);

  EXPECT_EQ(resolver.diagnostics()[1].severity, DiagnosticSeverity::kNote);
  EXPECT_EQ(resolver.diagnostics()[1].message, "previous declaration is here");
  EXPECT_EQ(resolver.diagnostics()[1].position.start.line, 1);
  EXPECT_EQ(resolver.diagnostics()[1].position.start.column, 16);
}


TEST(ResolverTest, VisitFileAST_MethodAndField) {
  auto source = "class Person { name: string; def get_name() { return name } }";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_FALSE(resolver.has_diagnostics());

  auto& declarations = file_ast->declarations();
  ASSERT_EQ(declarations.size(), 1u);

  auto class_decl = declarations[0]->As<ClassDeclaration>();

  ASSERT_EQ(class_decl->fields().size(), 1u);
  auto field_symbol = class_decl->fields()[0]->symbol();

  ASSERT_EQ(class_decl->declarations().size(), 1u);
  auto method_decl = class_decl->declarations()[0]->As<FunctionDeclaration>();
  auto method_func = method_decl->func();

  auto& statements = method_func->body()->statements();
  ASSERT_EQ(statements.size(), 1u);

  auto return_stmt = statements[0]->As<ReturnStatement>();
  auto return_expr = return_stmt->expr();

  auto ident_expr = return_expr->As<IdentifierExpression>();
  EXPECT_EQ(ident_expr->symbol(), field_symbol);
}


#if 0
TEST(ResolverTest, VisitFileAST_InnerClassInClass) {
  auto source = R"(
    class Outer {
      outer_field: int

      class Inner {
        inner_field: int

        def get_outer_field() {
          return outer_field
        }
      }
    }
  )";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_FALSE(resolver.has_diagnostics());

  auto& declarations = file_ast->declarations();
  ASSERT_EQ(declarations.size(), 1u);

  auto outer_class_decl = declarations[0]->As<ClassDeclaration>();
  ASSERT_EQ(outer_class_decl->fields().size(), 1u);
  ASSERT_EQ(outer_class_decl->declarations().size(), 1u);
  auto outer_field_symbol = outer_class_decl->fields()[0]->symbol();

  auto inner_class_decl = outer_class_decl->declarations()[0]->As<ClassDeclaration>();
  ASSERT_EQ(inner_class_decl->fields().size(), 1u);
  ASSERT_EQ(inner_class_decl->declarations().size(), 1u);

  auto get_outer_field_decl = inner_class_decl->declarations()[0]->As<FunctionDeclaration>();
  auto get_outer_field_func = get_outer_field_decl->func();
  auto& statements = get_outer_field_func->body()->statements();
  ASSERT_EQ(statements.size(), 1u);

  auto return_stmt = statements[0]->As<ReturnStatement>();
  auto return_expr = return_stmt->expr();
  auto ident_expr = return_expr->As<IdentifierExpression>();
  EXPECT_EQ(ident_expr->symbol(), outer_field_symbol);
}


TEST(ResolverTest, VisitFileAST_InnerClassInFunc) {
  auto source = R"(
    def outer_func(outer_param) {
      class Inner {
        def get_outer_param() {
          return outer_param
        }
      }
    }
  )";

  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  Resolver resolver(&context);
  resolver.VisitFileAST(file_ast.get());
  ASSERT_FALSE(resolver.has_diagnostics());

  auto& declarations = file_ast->declarations();
  ASSERT_EQ(declarations.size(), 1u);

  auto outer_func_decl = declarations[0]->As<FunctionDeclaration>();
  auto outer_func = outer_func_decl->func();
  ASSERT_EQ(outer_func->params().size(), 1u);
  ASSERT_EQ(outer_func->body()->declarations().size(), 1u);
  auto outer_param_symbol = outer_func->params()[0]->symbol();

  auto inner_class_decl = outer_func->body()->declarations()[0]->As<ClassDeclaration>();
  ASSERT_EQ(inner_class_decl->declarations().size(), 1u);

  auto get_outer_param_decl = inner_class_decl->declarations()[0]->As<FunctionDeclaration>();
  auto get_outer_param_func = get_outer_param_decl->func();
  auto& statements = get_outer_param_func->body()->statements();
  ASSERT_EQ(statements.size(), 1u);

  auto return_stmt = statements[0]->As<ReturnStatement>();
  auto return_expr = return_stmt->expr();
  auto ident_expr = return_expr->As<IdentifierExpression>();
  EXPECT_EQ(ident_expr->symbol(), outer_param_symbol);

  EXPECT_TRUE(outer_param_symbol->is_captured());
  EXPECT_EQ(outer_param_symbol->captured_index(), 0);
}
#endif


}  // namespace xylo
