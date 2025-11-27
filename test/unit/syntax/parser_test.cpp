
#include "xylo/syntax/parser.h"

#include <gtest/gtest.h>

#include "xylo/syntax/lexer.h"

namespace xylo {

static_assert(!std::copyable<Parser>);
static_assert(!std::movable<Parser>);


TEST(ParserTest, ParseFileAST_EmptyFile) {
  auto source = "";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());
  EXPECT_EQ(file_ast->declarations().size(), 0u);
}


TEST(ParserTest, EmptyClass) {
  auto source = "class Foo { }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  ASSERT_EQ(file_ast->declarations().size(), 1u);
  auto decl = file_ast->declarations()[0].get();
  auto class_decl = decl->As<ClassDeclaration>();

  EXPECT_EQ(class_decl->symbol()->name()->str().cpp_str(), "Foo");
  EXPECT_EQ(class_decl->fields().size(), 0u);
}


TEST(ParserTest, BasicClass) {
  auto source = "class Point { x : int; y : int; }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  ASSERT_EQ(file_ast->declarations().size(), 1u);
  auto decl = file_ast->declarations()[0].get();
  auto class_decl = decl->As<ClassDeclaration>();

  EXPECT_EQ(class_decl->symbol()->name()->str().cpp_str(), "Point");
  ASSERT_EQ(class_decl->fields().size(), 2u);
  EXPECT_EQ(class_decl->fields()[0]->symbol()->name()->str().cpp_str(), "x");
  EXPECT_EQ(class_decl->fields()[1]->symbol()->name()->str().cpp_str(), "y");
  EXPECT_EQ(class_decl->fields()[0]->type_repr()->As<NamedTypeRepr>()->name()->str().cpp_str(), "int");
  EXPECT_EQ(class_decl->fields()[1]->type_repr()->As<NamedTypeRepr>()->name()->str().cpp_str(), "int");
}


TEST(ParserTest, InvalidClassField) {
  auto source = "class Foo { 42; def foo() {} }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  parser.ParseFile();
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "unexpected integer literal");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 13);
}


TEST(ParserTest, InvalidClassMethod) {
  auto source = "class Foo { def 1() {} }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  parser.ParseFile();
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected identifier, found integer literal");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 17);
}


TEST(ParserTest, BasicFunction) {
  auto source = "def foo() { return true; }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  auto file_ast = parser.ParseFile();
  ASSERT_FALSE(parser.has_diagnostics());

  ASSERT_EQ(file_ast->declarations().size(), 1u);
  auto decl = file_ast->declarations()[0].get();
  auto func_decl = decl->As<FunctionDeclaration>();

  EXPECT_EQ(func_decl->symbol()->name()->str().cpp_str(), "foo");
  EXPECT_EQ(func_decl->func()->params().size(), 0u);
  EXPECT_EQ(func_decl->func()->body()->declarations().size(), 0u);
  ASSERT_EQ(func_decl->func()->body()->statements().size(), 1u);
  auto stmt = func_decl->func()->body()->statements()[0].get();

  auto return_stmt = stmt->As<ReturnStatement>();
  auto bool_literal = return_stmt->expr()->As<BooleanLiteral>();
  EXPECT_EQ(bool_literal->value(), true);
}


TEST(ParserTest, ParseFile_UnexpectedNewline) {
  auto source = "def foo\n() { return true; }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  parser.ParseFile();
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);
  EXPECT_EQ(parser.diagnostics()[0].message, "expected '(', found newline");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 8);
}


TEST(ParserTest, ParseFile_MissingLambda) {
  auto source = "def foo\ndef bar() { }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  parser.ParseFile();
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);
  EXPECT_EQ(parser.diagnostics()[0].message, "expected '(', found newline");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 8);
}


TEST(ParserTest, ParseFile_MissingFunctionBody) {
  auto source = "def foo(a, b)\ndef bar() { }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  parser.ParseFile();
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);
  EXPECT_EQ(parser.diagnostics()[0].message, "unexpected newline");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 14);
}


TEST(ParserTest, ParseFile_SyntaxError) {
  auto source = "let x = 42;";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  parser.ParseFile();
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);


  EXPECT_EQ(parser.diagnostics()[0].message, "unexpected 'let'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 1);
}


TEST(ParserTest, ParseFile_UnexpectedIdentifier) {
  auto source = "foo";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  parser.ParseFile();
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "unexpected identifier 'foo'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 1);
}


TEST(ParserTest, ParseFile_UnexpectedLiteral) {
  auto source = "42";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  parser.ParseFile();
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "unexpected integer literal");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 1);
}


TEST(ParserTest, ParseFile_UnrecognizedCharacter) {
  auto source = "def foo@ @ ) { }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  parser.ParseFile();
  ASSERT_TRUE(parser.has_diagnostics());
  ASSERT_EQ(parser.diagnostics().size(), 2);

  EXPECT_EQ(parser.diagnostics()[0].message, "unrecognized character");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 8);

  EXPECT_EQ(parser.diagnostics()[1].message, "expected '(', found ')'");
  EXPECT_EQ(parser.diagnostics()[1].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[1].position.start.column, 12);
}


TEST(ParserTest, ParseFunctionDeclaration_NoParams_NoBody) {
  auto source = "def foo() { }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto decl = parser.ParseFunctionDeclaration(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());

  auto func_decl = decl.get()->As<FunctionDeclaration>();

  EXPECT_EQ(func_decl->symbol()->name()->str().cpp_str(), "foo");
  EXPECT_EQ(func_decl->func()->params().size(), 0u);
  EXPECT_EQ(func_decl->func()->body()->declarations().size(), 0u);
  EXPECT_EQ(func_decl->func()->body()->statements().size(), 0u);
}


TEST(ParserTest, ParseFunctionDeclaration_WithParams_WithBody) {
  auto source = "def apply(f, x) { return f(x); }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto decl = parser.ParseFunctionDeclaration(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());

  auto func_decl = decl.get()->As<FunctionDeclaration>();

  EXPECT_EQ(func_decl->symbol()->name()->str().cpp_str(), "apply");
  ASSERT_EQ(func_decl->func()->params().size(), 2u);
  EXPECT_EQ(func_decl->func()->params()[0]->symbol()->name()->str().cpp_str(), "f");
  EXPECT_EQ(func_decl->func()->params()[1]->symbol()->name()->str().cpp_str(), "x");
  EXPECT_EQ(func_decl->func()->body()->declarations().size(), 0u);

  ASSERT_EQ(func_decl->func()->body()->statements().size(), 1u);
  auto stmt = func_decl->func()->body()->statements()[0].get();

  auto return_stmt = stmt->As<ReturnStatement>();
  auto apply_expr = return_stmt->expr()->As<ApplyExpression>();

  auto callee_expr = apply_expr->func()->As<IdentifierExpression>();
  EXPECT_EQ(callee_expr->name()->str().cpp_str(), "f");

  auto args_tuple = apply_expr->args();
  ASSERT_EQ(args_tuple->elements().size(), 1u);
  auto arg_expr = args_tuple->elements()[0].get();

  auto identifier_expr = arg_expr->As<IdentifierExpression>();
  EXPECT_EQ(identifier_expr->name()->str().cpp_str(), "x");
}


TEST(ParserTest, ParseFunctionDeclaration_NestedFunction) {
  auto source = "def outer() { def inner(x) { return x; } }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto decl = parser.ParseFunctionDeclaration(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());

  auto outer_func_decl = decl.get()->As<FunctionDeclaration>();

  EXPECT_EQ(outer_func_decl->symbol()->name()->str().cpp_str(), "outer");
  EXPECT_EQ(outer_func_decl->func()->params().size(), 0u);

  auto& outer_body_decls = outer_func_decl->func()->body()->declarations();
  ASSERT_EQ(outer_body_decls.size(), 1u);

  auto inner_func_decl = outer_body_decls[0]->As<FunctionDeclaration>();

  EXPECT_EQ(inner_func_decl->symbol()->name()->str().cpp_str(), "inner");
  ASSERT_EQ(inner_func_decl->func()->params().size(), 1u);
  EXPECT_EQ(inner_func_decl->func()->params()[0]->symbol()->name()->str().cpp_str(), "x");
  EXPECT_EQ(inner_func_decl->func()->body()->declarations().size(), 0u);
  ASSERT_EQ(inner_func_decl->func()->body()->statements().size(), 1u);
  auto stmt = inner_func_decl->func()->body()->statements()[0].get();

  auto return_stmt = stmt->As<ReturnStatement>();
  auto identifier_expr = return_stmt->expr()->As<IdentifierExpression>();
  EXPECT_EQ(identifier_expr->name()->str().cpp_str(), "x");
}


TEST(ParserTest, ParseFunctionDeclaration_SyntaxError) {
  auto source = "def foo( { ";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseFunctionDeclaration(scope.get());
  ASSERT_GE(parser.diagnostics().size(), 2);
  EXPECT_EQ(parser.diagnostics().size(), 2);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected ')', found '{'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 10);

  EXPECT_EQ(parser.diagnostics()[1].message, "expected '}', found eof");
  EXPECT_EQ(parser.diagnostics()[1].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[1].position.start.column, 12);
}


TEST(ParserTest, ParseFunctionDeclaration_MissingFuncName) {
  auto source = "def (x) { }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseFunctionDeclaration(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected identifier, found '('");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 5);
}


TEST(ParserTest, ParseFunctionDeclaration_MissingParam) {
  auto source = "def foo(x, ) { }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseFunctionDeclaration(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected identifier, found ')'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 12);
}


TEST(ParserTest, ParseFunctionDeclaration_InvalidParam) {
  auto source = "def foo(, 10) { }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseFunctionDeclaration(scope.get());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected identifier, found ','");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 9);
}


TEST(ParserTest, ParseBlock_EmptyBlock) {
  auto source = "{ ; }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto block = parser.ParseBlock(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());

  EXPECT_EQ(block->declarations().size(), 0u);
  EXPECT_EQ(block->statements().size(), 0u);
}


TEST(ParserTest, ParseBlock_SyntaxError) {
  auto source = "{ f x }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseBlock(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected ';' or newline, found identifier 'x'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 5);
}


TEST(ParserTest, ParseBlock_UnexpectedToken) {
  auto source = "{ a + : b; }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseBlock(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "unexpected ':'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 7);
}


TEST(ParserTest, ParseBlock_MissingLetVarName) {
  auto source = "{ let = 42; }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseBlock(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected identifier, found '='");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 7);
}


TEST(ParserTest, ParseBlock_MissingRBrace) {
  auto source = "{ return 0; )";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseBlock(scope.get());
  ASSERT_GE(parser.diagnostics().size(), 2);
  EXPECT_EQ(parser.diagnostics().size(), 2);

  EXPECT_EQ(parser.diagnostics()[0].message, "unexpected ')'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 13);

  EXPECT_EQ(parser.diagnostics()[1].message, "expected '}', found eof");
  EXPECT_EQ(parser.diagnostics()[1].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[1].position.start.column, 14);
}


TEST(ParserTest, ParseBlock_MissingRParen) {
  auto source = "{ foo(42 }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseBlock(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected ')', found '}'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 10);
}


TEST(ParserTest, ParseExpression_Precedence) {
  auto source = "a + b * c < -d(42) + (e << f)";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());

  auto less_expr = expr->As<BinaryExpression>();
  EXPECT_EQ(less_expr->op(), Token::kLT);

  auto add_expr = less_expr->lhs()->As<BinaryExpression>();
  EXPECT_EQ(add_expr->op(), Token::kAdd);

  auto a_expr = add_expr->lhs()->As<IdentifierExpression>();
  EXPECT_EQ(a_expr->name()->str().cpp_str(), "a");

  auto mul_expr = add_expr->rhs()->As<BinaryExpression>();
  EXPECT_EQ(mul_expr->op(), Token::kMul);

  auto b_expr = mul_expr->lhs()->As<IdentifierExpression>();
  EXPECT_EQ(b_expr->name()->str().cpp_str(), "b");

  auto c_expr = mul_expr->rhs()->As<IdentifierExpression>();
  EXPECT_EQ(c_expr->name()->str().cpp_str(), "c");

  auto add2_expr = less_expr->rhs()->As<BinaryExpression>();
  EXPECT_EQ(add2_expr->op(), Token::kAdd);

  auto neg_expr = add2_expr->lhs()->As<UnaryExpression>();
  EXPECT_EQ(neg_expr->op(), Token::kSub);

  auto apply_expr = neg_expr->operand()->As<ApplyExpression>();

  auto d_expr = apply_expr->func()->As<IdentifierExpression>();
  EXPECT_EQ(d_expr->name()->str().cpp_str(), "d");

  auto arg_tuple = apply_expr->args();
  ASSERT_EQ(arg_tuple->elements().size(), 1u);
  auto inner_arg = arg_tuple->elements()[0].get();

  ASSERT_EQ(inner_arg->type(), context.int_type());
  auto x_expr = inner_arg->As<IntegerLiteral>();
  EXPECT_EQ(x_expr->value(), 42);

  auto shl_expr = add2_expr->rhs()->As<BinaryExpression>();
  EXPECT_EQ(shl_expr->op(), Token::kShl);

  auto e_expr = shl_expr->lhs()->As<IdentifierExpression>();
  EXPECT_EQ(e_expr->name()->str().cpp_str(), "e");

  auto f_expr = shl_expr->rhs()->As<IdentifierExpression>();
  EXPECT_EQ(f_expr->name()->str().cpp_str(), "f");
}


TEST(ParserTest, ParseExpression_Ternary) {
  auto source = "x = a ? b + c : s == t ? u : v";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());

  auto assign_expr = expr->As<BinaryExpression>();

  auto x_expr = assign_expr->lhs()->As<IdentifierExpression>();
  EXPECT_EQ(x_expr->name()->str().cpp_str(), "x");

  auto ternary_expr = assign_expr->rhs()->As<ConditionalExpression>();

  auto a_expr = ternary_expr->cond()->As<IdentifierExpression>();
  EXPECT_EQ(a_expr->name()->str().cpp_str(), "a");

  auto add_expr = ternary_expr->then_expr()->As<BinaryExpression>();
  EXPECT_EQ(add_expr->op(), Token::kAdd);
  auto b_expr = add_expr->lhs()->As<IdentifierExpression>();
  EXPECT_EQ(b_expr->name()->str().cpp_str(), "b");
  auto c_expr = add_expr->rhs()->As<IdentifierExpression>();
  EXPECT_EQ(c_expr->name()->str().cpp_str(), "c");

  auto nested_ternary_expr = ternary_expr->else_expr()->As<ConditionalExpression>();

  auto eq_expr = nested_ternary_expr->cond()->As<BinaryExpression>();
  EXPECT_EQ(eq_expr->op(), Token::kEQ);
  auto s_expr = eq_expr->lhs()->As<IdentifierExpression>();
  EXPECT_EQ(s_expr->name()->str().cpp_str(), "s");
  auto t_expr = eq_expr->rhs()->As<IdentifierExpression>();
  EXPECT_EQ(t_expr->name()->str().cpp_str(), "t");

  auto u_expr = nested_ternary_expr->then_expr()->As<IdentifierExpression>();
  EXPECT_EQ(u_expr->name()->str().cpp_str(), "u");

  auto v_expr = nested_ternary_expr->else_expr()->As<IdentifierExpression>();
  EXPECT_EQ(v_expr->name()->str().cpp_str(), "v");
}


TEST(ParserTest, ParseExpression_MultiArguments) {
  auto source = "sum(1, 2, 3)";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  EXPECT_FALSE(parser.has_diagnostics());

  auto apply_expr = expr->As<ApplyExpression>();

  auto sum_expr = apply_expr->func()->As<IdentifierExpression>();
  EXPECT_EQ(sum_expr->name()->str().cpp_str(), "sum");

  auto arg_tuple = apply_expr->args();
  ASSERT_EQ(arg_tuple->elements().size(), 3u);

  for (size_t i = 0; i < 3; ++i) {
    auto int_expr = arg_tuple->elements()[i]->As<IntegerLiteral>();
    EXPECT_EQ(int_expr->value(), static_cast<int64_t>(i + 1));
  }
}


TEST(ParserTest, ParseExpression_FunctionReturnsFunction) {
  auto source = "make_adder(10)(20)";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());

  auto outer_apply = expr->As<ApplyExpression>();
  auto inner_apply = outer_apply->func()->As<ApplyExpression>();

  auto make_adder_expr = inner_apply->func()->As<IdentifierExpression>();
  EXPECT_EQ(make_adder_expr->name()->str().cpp_str(), "make_adder");

  auto inner_arg_tuple = inner_apply->args();
  ASSERT_EQ(inner_arg_tuple->elements().size(), 1u);

  auto ten_expr = inner_arg_tuple->elements()[0]->As<IntegerLiteral>();
  EXPECT_EQ(ten_expr->value(), 10);

  auto outer_arg_tuple = outer_apply->args();
  ASSERT_EQ(outer_arg_tuple->elements().size(), 1u);

  auto twenty_expr = outer_arg_tuple->elements()[0]->As<IntegerLiteral>();
  EXPECT_EQ(twenty_expr->value(), 20);
}


TEST(ParserTest, ParseExpression_InvalidParen) {
  auto source = "1 + (1 + 2 *)";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "unexpected ')'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 13);
}


TEST(ParserTest, ParseExpression_New) {
  auto source = "new Foo{ x: 10, y: 20 }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());

  auto new_expr = expr->As<NewExpression>();
  auto& init_list = new_expr->field_initializers();
  ASSERT_EQ(init_list.size(), 2u);

  EXPECT_EQ(init_list[0]->field_name()->str().cpp_str(), "x");
  auto x_value = init_list[0]->expr()->As<IntegerLiteral>();
  EXPECT_EQ(x_value->value(), 10);

  EXPECT_EQ(init_list[1]->field_name()->str().cpp_str(), "y");
  auto y_value = init_list[1]->expr()->As<IntegerLiteral>();
  EXPECT_EQ(y_value->value(), 20);
}


TEST(ParserTest, ParseExpression_New_MissingFieldName) {
  auto source = "new Foo{ : 10 }; a = b";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseExpression(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected identifier or '}', found ':'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 10);
}


TEST(ParserTest, ParseExpression_New_MissingColon) {
  auto source = "new Foo{ x 10 }; a = b";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseExpression(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected ':', found integer literal");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 12);
}


TEST(ParserTest, ParseExpression_New_WrongBrace) {
  auto source = "new Foo[ x: 10 ]; a = b";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseExpression(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  ASSERT_EQ(parser.diagnostics().size(), 2);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected '{', found '['");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 8);

  EXPECT_EQ(parser.diagnostics()[1].message, "expected identifier or '}', found '['");
  EXPECT_EQ(parser.diagnostics()[1].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[1].position.start.column, 8);
}


TEST(ParserTest, ParseExpression_BinaryNew) {
  auto source = "(a = new Foo{})";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());
}


TEST(ParserTest, ParseExpression_TernaryNew) {
  auto source = "(cond ? new Foo{} : new Bar{})";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());
}


TEST(ParserTest, ParseExpression_PrefixNew) {
  auto source = "(!new Foo{})";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());
}


TEST(ParserTest, ParseExpression_PostfixNew) {
  auto source = "(new Foo{}.bar)";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());
}


TEST(ParserTest, ParseExpression_BinaryFn) {
  auto source = "(a = fn(x) { return x })";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());
}


TEST(ParserTest, ParseExpression_TernaryFn) {
  auto source = "(cond ? fn(x) { return x } : fn(y) { return y })";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());
}


TEST(ParserTest, ParseExpression_PrefixFn) {
  auto source = "(!fn(x) { return x })";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());
}


TEST(ParserTest, ParseExpression_PostfixFn) {
  auto source = "(fn(x) { return x }(1))";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());
}


TEST(ParserTest, ParseExpression_FnMissingParam) {
  auto source = "(fn(x,) { })";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseExpression(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected identifier, found ')'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 7);
}


TEST(ParserTest, ParseExpression_FnMissingParen) {
  auto source = "(fn x => x)";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseExpression(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 1);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected '(', found identifier 'x'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 5);
}


TEST(ParserTest, ParseExpression_IfExpr) {
  auto source = "if (cond) a else b";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());

  auto if_expr = expr->As<ConditionalExpression>();
  auto cond_expr = if_expr->cond()->As<IdentifierExpression>();
  EXPECT_EQ(cond_expr->name()->str().cpp_str(), "cond");

  auto then_expr = if_expr->then_expr()->As<IdentifierExpression>();
  EXPECT_EQ(then_expr->name()->str().cpp_str(), "a");

  auto else_expr = if_expr->else_expr()->As<IdentifierExpression>();
  EXPECT_EQ(else_expr->name()->str().cpp_str(), "b");
}


TEST(ParserTest, ParseExpression_IfExpr_MissingParen) {
  auto source = "if cond a else b";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  parser.ParseExpression(scope.get());
  ASSERT_TRUE(parser.has_diagnostics());
  EXPECT_EQ(parser.diagnostics().size(), 2);

  EXPECT_EQ(parser.diagnostics()[0].message, "expected '(', found identifier 'cond'");
  EXPECT_EQ(parser.diagnostics()[0].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[0].position.start.column, 4);

  EXPECT_EQ(parser.diagnostics()[1].message, "expected ')', found identifier 'a'");
  EXPECT_EQ(parser.diagnostics()[1].position.start.line, 1);
  EXPECT_EQ(parser.diagnostics()[1].position.start.column, 9);
}


TEST(ParserTest, ParseExpression_IfExpr_WithBLock) {
  auto source = "if (cond) { return a } else { return b }";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);
  auto scope = Scope::CreateRoot();

  auto expr = parser.ParseExpression(scope.get());
  ASSERT_FALSE(parser.has_diagnostics());

  auto if_expr = expr->As<ConditionalExpression>();
  auto cond_expr = if_expr->cond()->As<IdentifierExpression>();
  EXPECT_EQ(cond_expr->name()->str().cpp_str(), "cond");

  auto then_block = if_expr->then_expr()->As<BlockExpression>();
  ASSERT_EQ(then_block->block()->statements().size(), 1u);
  auto then_return_stmt = then_block->block()->statements()[0]->As<ReturnStatement>();
  auto then_return_expr = then_return_stmt->expr()->As<IdentifierExpression>();
  EXPECT_EQ(then_return_expr->name()->str().cpp_str(), "a");

  auto else_block = if_expr->else_expr()->As<BlockExpression>();
  ASSERT_EQ(else_block->block()->statements().size(), 1u);
  auto else_return_stmt = else_block->block()->statements()[0]->As<ReturnStatement>();
  auto else_return_expr = else_return_stmt->expr()->As<IdentifierExpression>();
  EXPECT_EQ(else_return_expr->name()->str().cpp_str(), "b");
}


TEST(ParserTest, ParseTypeRepr_Single) {
  auto source = "Foo,";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  auto type_repr = parser.ParseTypeRepr();
  ASSERT_FALSE(parser.has_diagnostics());

  auto ident_type_repr = type_repr->As<NamedTypeRepr>();
  EXPECT_EQ(ident_type_repr->name()->str().cpp_str(), "Foo");
}


TEST(ParserTest, ParseTypeRepr_RegularFunc) {
  auto source = "A -> B -> C";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  auto type_repr = parser.ParseTypeRepr();
  ASSERT_FALSE(parser.has_diagnostics());

  auto func_type_repr = type_repr->As<FunctionTypeRepr>();
  auto params_type = func_type_repr->params_type()->As<TupleTypeRepr>();
  ASSERT_EQ(params_type->elements().size(), 1u);
  auto param_type = params_type->elements()[0]->As<NamedTypeRepr>();
  EXPECT_EQ(param_type->name()->str().cpp_str(), "A");

  auto return_type_repr = func_type_repr->return_type()->As<FunctionTypeRepr>();
  auto return_params_type = return_type_repr->params_type()->As<TupleTypeRepr>();
  ASSERT_EQ(return_params_type->elements().size(), 1u);
  auto return_param_type = return_params_type->elements()[0]->As<NamedTypeRepr>();
  EXPECT_EQ(return_param_type->name()->str().cpp_str(), "B");

  auto return_return_type = return_type_repr->return_type()->As<NamedTypeRepr>();
  EXPECT_EQ(return_return_type->name()->str().cpp_str(), "C");
}


TEST(ParserTest, ParseTypeRepr_FuncPrec) {
  auto source = "(A -> B) -> C";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  auto type_repr = parser.ParseTypeRepr();
  ASSERT_FALSE(parser.has_diagnostics());

  auto func_type_repr = type_repr->As<FunctionTypeRepr>();
  auto params_type = func_type_repr->params_type()->As<TupleTypeRepr>();
  ASSERT_EQ(params_type->elements().size(), 1u);
  auto param_func_type = params_type->elements()[0]->As<FunctionTypeRepr>();

  auto param_params_type = param_func_type->params_type()->As<TupleTypeRepr>();
  ASSERT_EQ(param_params_type->elements().size(), 1u);
  auto param_type = param_params_type->elements()[0]->As<NamedTypeRepr>();
  EXPECT_EQ(param_type->name()->str().cpp_str(), "A");

  auto param_return_type = param_func_type->return_type()->As<NamedTypeRepr>();
  EXPECT_EQ(param_return_type->name()->str().cpp_str(), "B");

  auto return_type = func_type_repr->return_type()->As<NamedTypeRepr>();
  EXPECT_EQ(return_type->name()->str().cpp_str(), "C");
}


TEST(ParserTest, ParseTypeRepr_MultiParamFunc) {
  auto source = "(A, B, C) -> D";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  auto type_repr = parser.ParseTypeRepr();
  ASSERT_FALSE(parser.has_diagnostics());

  auto func_type_repr = type_repr->As<FunctionTypeRepr>();
  auto params_type = func_type_repr->params_type()->As<TupleTypeRepr>();
  ASSERT_EQ(params_type->elements().size(), 3u);

  auto param_type_a = params_type->elements()[0]->As<NamedTypeRepr>();
  EXPECT_EQ(param_type_a->name()->str().cpp_str(), "A");

  auto param_type_b = params_type->elements()[1]->As<NamedTypeRepr>();
  EXPECT_EQ(param_type_b->name()->str().cpp_str(), "B");

  auto param_type_c = params_type->elements()[2]->As<NamedTypeRepr>();
  EXPECT_EQ(param_type_c->name()->str().cpp_str(), "C");

  auto return_type = func_type_repr->return_type()->As<NamedTypeRepr>();
  EXPECT_EQ(return_type->name()->str().cpp_str(), "D");
}


TEST(ParserTest, ParseTypeRepr_FuncParamFunc) {
  auto source = "(A -> B, C) -> D";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  auto type_repr = parser.ParseTypeRepr();
  ASSERT_FALSE(parser.has_diagnostics());

  auto func_type_repr = type_repr->As<FunctionTypeRepr>();
  auto params_type = func_type_repr->params_type()->As<TupleTypeRepr>();
  ASSERT_EQ(params_type->elements().size(), 2u);

  auto param_type_func = params_type->elements()[0]->As<FunctionTypeRepr>();
  auto param_func_params_type = param_type_func->params_type()->As<TupleTypeRepr>();
  ASSERT_EQ(param_func_params_type->elements().size(), 1u);
  auto param_func_param_type = param_func_params_type->elements()[0]->As<NamedTypeRepr>();
  EXPECT_EQ(param_func_param_type->name()->str().cpp_str(), "A");

  auto param_func_return_type = param_type_func->return_type()->As<NamedTypeRepr>();
  EXPECT_EQ(param_func_return_type->name()->str().cpp_str(), "B");

  auto param_type_c = params_type->elements()[1]->As<NamedTypeRepr>();
  EXPECT_EQ(param_type_c->name()->str().cpp_str(), "C");

  auto return_type = func_type_repr->return_type()->As<NamedTypeRepr>();
  EXPECT_EQ(return_type->name()->str().cpp_str(), "D");
}


TEST(ParserTest, ParseTypeRepr_VoidParam) {
  auto source = "() -> A";
  XyloContext context;
  Lexer lexer(&context, source);
  Parser parser(&context, &lexer);

  auto type_repr = parser.ParseTypeRepr();
  ASSERT_FALSE(parser.has_diagnostics());

  auto func_type_repr = type_repr->As<FunctionTypeRepr>();
  auto params_type = func_type_repr->params_type()->As<TupleTypeRepr>();
  ASSERT_EQ(params_type->elements().size(), 0u);

  auto return_type = func_type_repr->return_type()->As<NamedTypeRepr>();
  EXPECT_EQ(return_type->name()->str().cpp_str(), "A");
}


}  // namespace xylo
