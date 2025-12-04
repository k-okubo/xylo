
#include "xylo/syntax/type.h"

#include <gtest/gtest.h>

#include "xylo/syntax/context.h"
#include "xylo/syntax/substitution.h"

namespace xylo {

static_assert(!std::copyable<Type>);
static_assert(!std::movable<Type>);


class TestTypes {
 public:
  TestTypes() :
      context_(),
      foo_type_(NominalType::Category::kClass, context_.InternIdentifier("Foo")),
      base1_type_(NominalType::Category::kClass, context_.InternIdentifier("Base1")),
      base2_type_(NominalType::Category::kClass, context_.InternIdentifier("Base2")),
      bar_type_(NominalType::Category::kClass, context_.InternIdentifier("Bar")),
      baz_type_(NominalType::Category::kClass, context_.InternIdentifier("Baz")) {
    // inheritance relationships
    base2_type_.AddSuper(&base1_type_);
    bar_type_.AddSuper(&base2_type_);
    baz_type_.AddSuper(&base2_type_);
  }

  XyloContext* context() { return &context_; }
  NominalType* foo_type() { return &foo_type_; }
  NominalType* base1_type() { return &base1_type_; }
  NominalType* base2_type() { return &base2_type_; }
  NominalType* bar_type() { return &bar_type_; }
  NominalType* baz_type() { return &baz_type_; }

 private:
  XyloContext context_;
  NominalType foo_type_;
  NominalType base1_type_;
  NominalType base2_type_;
  NominalType bar_type_;
  NominalType baz_type_;
};


TEST(TypeTest, NominalSubtyping) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto base1_type = types.base1_type();
  auto bar_type = types.bar_type();

  EXPECT_TRUE(bar_type->IsSubtypeOf(bar_type));
  EXPECT_TRUE(bar_type->IsSubtypeOf(base1_type));
  EXPECT_FALSE(base1_type->IsSubtypeOf(bar_type));
  EXPECT_FALSE(bar_type->IsSubtypeOf(foo_type));

  EXPECT_TRUE(bar_type->CanConstrainSubtypeOf(bar_type));
  EXPECT_TRUE(bar_type->CanConstrainSubtypeOf(base1_type));
  EXPECT_FALSE(base1_type->CanConstrainSubtypeOf(bar_type));
  EXPECT_FALSE(bar_type->CanConstrainSubtypeOf(foo_type));

  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(bar_type));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(base1_type));
  EXPECT_FALSE(base1_type->ConstrainSubtypeOf(bar_type));
  EXPECT_FALSE(bar_type->ConstrainSubtypeOf(foo_type));
}


TEST(TypeTest, FunctionSubtyping) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto base1_type = types.base1_type();
  auto bar_type = types.bar_type();

  auto param_type_foo = new TupleType();
  param_type_foo->add_element(foo_type);

  auto param_type_bar = new TupleType();
  param_type_bar->add_element(bar_type);

  auto param_type_base1 = new TupleType();
  param_type_base1->add_element(base1_type);

  auto bar2foo = new FunctionType(false, param_type_bar, foo_type);
  auto bar2fooB = new FunctionType(false, param_type_bar, foo_type);
  auto base2foo = new FunctionType(false, param_type_base1, foo_type);
  auto foo2bar = new FunctionType(false, param_type_foo, bar_type);
  auto foo2base = new FunctionType(false, param_type_foo, base1_type);

  EXPECT_TRUE(bar2foo->IsSubtypeOf(bar2fooB));
  EXPECT_FALSE(bar2foo->IsSubtypeOf(base2foo));
  EXPECT_TRUE(base2foo->IsSubtypeOf(bar2foo));
  EXPECT_TRUE(foo2bar->IsSubtypeOf(foo2base));
  EXPECT_FALSE(foo2base->IsSubtypeOf(foo2bar));
  EXPECT_FALSE(bar2foo->IsSubtypeOf(foo2bar));
  EXPECT_FALSE(foo2bar->IsSubtypeOf(bar2foo));

  EXPECT_TRUE(bar2foo->CanConstrainSubtypeOf(bar2fooB));
  EXPECT_FALSE(bar2foo->CanConstrainSubtypeOf(base2foo));
  EXPECT_TRUE(base2foo->CanConstrainSubtypeOf(bar2foo));
  EXPECT_TRUE(foo2bar->CanConstrainSubtypeOf(foo2base));
  EXPECT_FALSE(foo2base->CanConstrainSubtypeOf(foo2bar));
  EXPECT_FALSE(bar2foo->CanConstrainSubtypeOf(foo2bar));
  EXPECT_FALSE(foo2bar->CanConstrainSubtypeOf(bar2foo));

  EXPECT_TRUE(bar2foo->ConstrainSubtypeOf(bar2fooB));
  EXPECT_FALSE(bar2foo->ConstrainSubtypeOf(base2foo));
  EXPECT_TRUE(base2foo->ConstrainSubtypeOf(bar2foo));
  EXPECT_TRUE(foo2bar->ConstrainSubtypeOf(foo2base));
  EXPECT_FALSE(foo2base->ConstrainSubtypeOf(foo2bar));
  EXPECT_FALSE(bar2foo->ConstrainSubtypeOf(foo2bar));
  EXPECT_FALSE(foo2bar->ConstrainSubtypeOf(bar2foo));

  delete param_type_foo;
  delete param_type_bar;
  delete param_type_base1;
  delete bar2foo;
  delete bar2fooB;
  delete base2foo;
  delete foo2bar;
  delete foo2base;
}


TEST(TypeTest, ClosureSubtyping) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto param_type_foo1 = new TupleType();
  param_type_foo1->add_element(foo_type);
  auto param_type_foo2 = new TupleType();
  param_type_foo2->add_element(foo_type);

  auto regular_func_type = new FunctionType(false, param_type_foo1, bar_type);
  auto closure_func_type = new FunctionType(true, param_type_foo2, bar_type);

  EXPECT_TRUE(regular_func_type->IsSubtypeOf(closure_func_type));
  EXPECT_FALSE(closure_func_type->IsSubtypeOf(regular_func_type));
  EXPECT_TRUE(closure_func_type->IsSubtypeOf(closure_func_type));

  EXPECT_TRUE(regular_func_type->CanConstrainSubtypeOf(closure_func_type));
  EXPECT_FALSE(closure_func_type->CanConstrainSubtypeOf(regular_func_type));
  EXPECT_TRUE(closure_func_type->CanConstrainSubtypeOf(closure_func_type));

  EXPECT_TRUE(regular_func_type->ConstrainSubtypeOf(closure_func_type));
  EXPECT_FALSE(closure_func_type->ConstrainSubtypeOf(regular_func_type));
  EXPECT_TRUE(closure_func_type->ConstrainSubtypeOf(closure_func_type));

  delete param_type_foo1;
  delete param_type_foo2;
  delete regular_func_type;
  delete closure_func_type;
}


TEST(TypeTest, TupleSubtyping) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto base1_type = types.base1_type();
  auto bar_type = types.bar_type();

  auto bar_foo = new TupleType();
  bar_foo->add_element(bar_type);
  bar_foo->add_element(foo_type);

  auto bar_fooB = new TupleType();
  bar_fooB->add_element(bar_type);
  bar_fooB->add_element(foo_type);

  auto base_foo = new TupleType();
  base_foo->add_element(base1_type);
  base_foo->add_element(foo_type);

  auto foo_bar = new TupleType();
  foo_bar->add_element(foo_type);
  foo_bar->add_element(bar_type);

  auto foo_base = new TupleType();
  foo_base->add_element(foo_type);
  foo_base->add_element(base1_type);

  auto foo_bar_bar = new TupleType();
  foo_bar_bar->add_element(foo_type);
  foo_bar_bar->add_element(bar_type);
  foo_bar_bar->add_element(bar_type);

  EXPECT_TRUE(bar_foo->IsSubtypeOf(bar_fooB));
  EXPECT_TRUE(bar_foo->IsSubtypeOf(base_foo));
  EXPECT_FALSE(base_foo->IsSubtypeOf(bar_foo));
  EXPECT_TRUE(foo_bar->IsSubtypeOf(foo_base));
  EXPECT_FALSE(foo_base->IsSubtypeOf(foo_bar));
  EXPECT_FALSE(bar_foo->IsSubtypeOf(foo_bar));
  EXPECT_FALSE(foo_bar->IsSubtypeOf(bar_foo));
  EXPECT_FALSE(foo_bar->IsSubtypeOf(foo_bar_bar));
  EXPECT_FALSE(foo_bar_bar->IsSubtypeOf(foo_bar));

  EXPECT_TRUE(bar_foo->CanConstrainSubtypeOf(bar_fooB));
  EXPECT_TRUE(bar_foo->CanConstrainSubtypeOf(base_foo));
  EXPECT_FALSE(base_foo->CanConstrainSubtypeOf(bar_foo));
  EXPECT_TRUE(foo_bar->CanConstrainSubtypeOf(foo_base));
  EXPECT_FALSE(foo_base->CanConstrainSubtypeOf(foo_bar));
  EXPECT_FALSE(bar_foo->CanConstrainSubtypeOf(foo_bar));
  EXPECT_FALSE(foo_bar->CanConstrainSubtypeOf(bar_foo));
  EXPECT_FALSE(foo_bar->CanConstrainSubtypeOf(foo_bar_bar));
  EXPECT_FALSE(foo_bar_bar->CanConstrainSubtypeOf(foo_bar));

  EXPECT_TRUE(bar_foo->ConstrainSubtypeOf(bar_fooB));
  EXPECT_TRUE(bar_foo->ConstrainSubtypeOf(base_foo));
  EXPECT_FALSE(base_foo->ConstrainSubtypeOf(bar_foo));
  EXPECT_TRUE(foo_bar->ConstrainSubtypeOf(foo_base));
  EXPECT_FALSE(foo_base->ConstrainSubtypeOf(foo_bar));
  EXPECT_FALSE(bar_foo->ConstrainSubtypeOf(foo_bar));
  EXPECT_FALSE(foo_bar->ConstrainSubtypeOf(bar_foo));
  EXPECT_FALSE(foo_bar->ConstrainSubtypeOf(foo_bar_bar));
  EXPECT_FALSE(foo_bar_bar->ConstrainSubtypeOf(foo_bar));

  delete bar_foo;
  delete bar_fooB;
  delete base_foo;
  delete foo_bar;
  delete foo_base;
  delete foo_bar_bar;
}


TEST(TypeTest, TopAndBottomSubtyping) {
  TestTypes types;
  auto foo_type = types.foo_type();

  auto top_type1 = new IntersectionType();
  auto top_type2 = new IntersectionType();
  auto bottom_type1 = new UnionType();
  auto bottom_type2 = new UnionType();

  EXPECT_TRUE(top_type1->IsSubtypeOf(top_type2));
  EXPECT_TRUE(bottom_type1->IsSubtypeOf(bottom_type2));

  EXPECT_TRUE(bottom_type1->IsSubtypeOf(top_type1));
  EXPECT_FALSE(top_type1->IsSubtypeOf(bottom_type1));

  EXPECT_TRUE(foo_type->IsSubtypeOf(top_type1));
  EXPECT_FALSE(top_type1->IsSubtypeOf(foo_type));

  EXPECT_TRUE(bottom_type1->IsSubtypeOf(foo_type));
  EXPECT_FALSE(foo_type->IsSubtypeOf(bottom_type1));

  delete top_type1;
  delete top_type2;
  delete bottom_type1;
  delete bottom_type2;
}


TEST(TypeTest, TypeMetavar_Linear_Single1) {
  TestTypes types;
  auto base2_type = types.base2_type();
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(base2_type));

  EXPECT_TRUE(bar_type->IsSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->IsSubtypeOf(metavar2));
  EXPECT_TRUE(bar_type->IsSubtypeOf(metavar3));
  EXPECT_FALSE(base2_type->IsSubtypeOf(metavar1));
  EXPECT_FALSE(base2_type->IsSubtypeOf(metavar2));
  EXPECT_FALSE(base2_type->IsSubtypeOf(metavar3));
  EXPECT_FALSE(metavar1->IsSubtypeOf(bar_type));
  EXPECT_FALSE(metavar2->IsSubtypeOf(bar_type));
  EXPECT_FALSE(metavar3->IsSubtypeOf(bar_type));
  EXPECT_TRUE(metavar1->IsSubtypeOf(base2_type));
  EXPECT_TRUE(metavar2->IsSubtypeOf(base2_type));
  EXPECT_TRUE(metavar3->IsSubtypeOf(base2_type));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), bar_type);

  TypePrinter vtp({.verbose = true});
  EXPECT_EQ(vtp(metavar1),
            "A' where Bar <: A' <: (B' & Base1 & Base2), "
            "(A' | Bar) <: B' <: (C' & Base1 & Base2), "
            "(B' | Bar) <: C' <: (Base1 & Base2)");

  TypePrinter stp;
  TypePrinter::NameMap name_map;
  EXPECT_EQ(stp(metavar1, &name_map), "A where Bar <: A <: B, B <: C, C <: Base2");
  EXPECT_EQ(stp(metavar2, &name_map), "B");
  EXPECT_EQ(stp(metavar3, &name_map), "C");

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Linear_Single2) {
  TestTypes types;
  auto base2_type = types.base2_type();
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(base2_type));
  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));

  EXPECT_TRUE(bar_type->IsSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->IsSubtypeOf(metavar2));
  EXPECT_TRUE(bar_type->IsSubtypeOf(metavar3));
  EXPECT_FALSE(base2_type->IsSubtypeOf(metavar1));
  EXPECT_FALSE(base2_type->IsSubtypeOf(metavar2));
  EXPECT_FALSE(base2_type->IsSubtypeOf(metavar3));
  EXPECT_FALSE(metavar1->IsSubtypeOf(bar_type));
  EXPECT_FALSE(metavar2->IsSubtypeOf(bar_type));
  EXPECT_FALSE(metavar3->IsSubtypeOf(bar_type));
  EXPECT_TRUE(metavar1->IsSubtypeOf(base2_type));
  EXPECT_TRUE(metavar2->IsSubtypeOf(base2_type));
  EXPECT_TRUE(metavar3->IsSubtypeOf(base2_type));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), bar_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Linear_Sibling1) {
  TestTypes types;
  auto bar_type = types.bar_type();
  auto baz_type = types.baz_type();
  auto base2_type = types.base2_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(baz_type->ConstrainSubtypeOf(metavar1));

  EXPECT_TRUE(bar_type->IsSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->IsSubtypeOf(metavar2));
  EXPECT_TRUE(bar_type->IsSubtypeOf(metavar3));
  EXPECT_TRUE(baz_type->IsSubtypeOf(metavar1));
  EXPECT_TRUE(baz_type->IsSubtypeOf(metavar2));
  EXPECT_TRUE(baz_type->IsSubtypeOf(metavar3));
  EXPECT_FALSE(base2_type->IsSubtypeOf(metavar1));
  EXPECT_FALSE(base2_type->IsSubtypeOf(metavar2));
  EXPECT_FALSE(base2_type->IsSubtypeOf(metavar3));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), base2_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Linear_Sibling2) {
  TestTypes types;
  auto bar_type = types.bar_type();
  auto baz_type = types.baz_type();
  auto base2_type = types.base2_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(baz_type->ConstrainSubtypeOf(metavar3));

  EXPECT_TRUE(bar_type->IsSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->IsSubtypeOf(metavar2));
  EXPECT_TRUE(bar_type->IsSubtypeOf(metavar3));
  EXPECT_FALSE(baz_type->IsSubtypeOf(metavar1));
  EXPECT_FALSE(baz_type->IsSubtypeOf(metavar2));
  EXPECT_TRUE(baz_type->IsSubtypeOf(metavar3));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), base2_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Linear_Unrelated1) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(foo_type->ConstrainSubtypeOf(metavar1));
  EXPECT_FALSE(bar_type->ConstrainSubtypeOf(metavar1));

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Linear_Unrelated2) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(foo_type->ConstrainSubtypeOf(metavar1));
  EXPECT_FALSE(bar_type->ConstrainSubtypeOf(metavar3));

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Linear_Unrelated3) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_FALSE(metavar1->ConstrainSubtypeOf(foo_type));

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Linear_Unrelated4) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_FALSE(metavar3->ConstrainSubtypeOf(foo_type));

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Circular_Vars) {
  auto metavarA = new TypeMetavar();
  auto metavarB1 = new TypeMetavar();
  auto metavarB2 = new TypeMetavar();

  TypePrinter printer({.verbose = true});

  EXPECT_TRUE(metavarA->ConstrainSubtypeOf(metavarA));
  EXPECT_EQ(printer(metavarA), "A'");

  EXPECT_TRUE(metavarB1->ConstrainSubtypeOf(metavarB2));
  EXPECT_TRUE(metavarB2->ConstrainSubtypeOf(metavarB1));
  EXPECT_EQ(printer(metavarB1), "A' where B' <: A' <: B', A' <: B' <: A'");

  delete metavarA;
  delete metavarB1;
  delete metavarB2;
}


TEST(TypeTest, TypeMetavar_Circular_Single) {
  TestTypes types;
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), bar_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Circular_Sibling1) {
  TestTypes types;
  auto bar_type = types.bar_type();
  auto baz_type = types.baz_type();
  auto base2_type = types.base2_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(baz_type->ConstrainSubtypeOf(metavar1));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), base2_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Circular_Sibling2) {
  TestTypes types;
  auto bar_type = types.bar_type();
  auto baz_type = types.baz_type();
  auto base2_type = types.base2_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(baz_type->ConstrainSubtypeOf(metavar3));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), base2_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Circular_Supertype1) {
  TestTypes types;
  auto bar_type = types.bar_type();
  auto base1_type = types.base1_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(base1_type));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), bar_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Circular_Supertype2) {
  TestTypes types;
  auto bar_type = types.bar_type();
  auto base1_type = types.base1_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(base1_type));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), bar_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Circular_Unrelated1) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(foo_type->ConstrainSubtypeOf(metavar1));
  EXPECT_FALSE(bar_type->ConstrainSubtypeOf(metavar1));

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Circular_Unrelated2) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(foo_type->ConstrainSubtypeOf(metavar1));
  EXPECT_FALSE(bar_type->ConstrainSubtypeOf(metavar3));

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Circular_Unrelated3) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_FALSE(metavar1->ConstrainSubtypeOf(foo_type));

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Circular_Unrelated4) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_FALSE(metavar3->ConstrainSubtypeOf(foo_type));

  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, TypeMetavar_Vshape_Single) {
  TestTypes types;
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();
  auto metavar4 = new TypeMetavar();
  auto metavar5 = new TypeMetavar();

  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar4->ConstrainSubtypeOf(metavar5));
  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar4));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar4->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar5->Zonk(&subst, true, &allocated), bar_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
  delete metavar4;
  delete metavar5;
}


TEST(TypeTest, TypeMetavar_Vshape_Sibling1) {
  TestTypes types;
  auto bar_type = types.bar_type();
  auto baz_type = types.baz_type();
  auto base2_type = types.base2_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();
  auto metavar4 = new TypeMetavar();
  auto metavar5 = new TypeMetavar();

  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar4->ConstrainSubtypeOf(metavar5));
  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar4));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(baz_type->ConstrainSubtypeOf(metavar1));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavar4->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavar5->Zonk(&subst, true, &allocated), base2_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
  delete metavar4;
  delete metavar5;
}


TEST(TypeTest, TypeMetavar_Vshape_Sibling2) {
  TestTypes types;
  auto bar_type = types.bar_type();
  auto baz_type = types.baz_type();
  auto base2_type = types.base2_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();
  auto metavar4 = new TypeMetavar();
  auto metavar5 = new TypeMetavar();

  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar4->ConstrainSubtypeOf(metavar5));
  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar4));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(baz_type->ConstrainSubtypeOf(metavar3));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavar4->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar5->Zonk(&subst, true, &allocated), bar_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
  delete metavar4;
  delete metavar5;
}


TEST(TypeTest, TypeMetavar_LambdaShape_Common) {
  TestTypes types;
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();
  auto metavar4 = new TypeMetavar();
  auto metavar5 = new TypeMetavar();

  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar4->ConstrainSubtypeOf(metavar5));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(metavar5->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar4));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar4->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar5->Zonk(&subst, true, &allocated), bar_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
  delete metavar4;
  delete metavar5;
}


TEST(TypeTest, TypeMetavar_LambdaShape_Sibling) {
  TestTypes types;
  auto bar_type = types.bar_type();
  auto baz_type = types.baz_type();
  auto base2_type = types.base2_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();
  auto metavar4 = new TypeMetavar();
  auto metavar5 = new TypeMetavar();

  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar4->ConstrainSubtypeOf(metavar5));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(metavar5->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(baz_type->ConstrainSubtypeOf(metavar4));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar3->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar4->Zonk(&subst, true, &allocated), baz_type);
  EXPECT_EQ(metavar5->Zonk(&subst, true, &allocated), baz_type);

  delete metavar1;
  delete metavar2;
  delete metavar3;
  delete metavar4;
  delete metavar5;
}


TEST(TypeTest, TypeMetavar_LambdaShape_Unrelated1) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();
  auto metavar4 = new TypeMetavar();
  auto metavar5 = new TypeMetavar();

  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar4->ConstrainSubtypeOf(metavar5));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(metavar5->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(foo_type->ConstrainSubtypeOf(metavar2));
  EXPECT_FALSE(bar_type->ConstrainSubtypeOf(metavar4));

  delete metavar1;
  delete metavar2;
  delete metavar3;
  delete metavar4;
  delete metavar5;
}


TEST(TypeTest, TypeMetavar_LambdaShape_Unrelated2) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();
  auto metavar4 = new TypeMetavar();
  auto metavar5 = new TypeMetavar();

  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar4->ConstrainSubtypeOf(metavar5));
  EXPECT_TRUE(foo_type->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar4));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_FALSE(metavar5->ConstrainSubtypeOf(metavar1));

  delete metavar1;
  delete metavar2;
  delete metavar3;
  delete metavar4;
  delete metavar5;
}


TEST(TypeTest, TypeMetavar_MultiSupertype) {
  for (int i = 0; i < 2; ++i) {
    auto foo_ident = new Identifier(HString("Foo"));
    auto bar_ident = new Identifier(HString("Bar"));
    auto baz_ident = new Identifier(HString("Baz"));
    auto baseA_ident = new Identifier(HString("BaseA"));
    auto baseB_ident = new Identifier(HString("BaseB"));
    auto foo_type = new NominalType(NominalType::Category::kClass, foo_ident);
    auto bar_type = new NominalType(NominalType::Category::kClass, bar_ident);
    auto baz_type = new NominalType(NominalType::Category::kClass, baz_ident);
    auto baseA_type = new NominalType(NominalType::Category::kClass, baseA_ident);
    auto baseB_type = new NominalType(NominalType::Category::kClass, baseB_ident);

    foo_type->AddSuper(baseA_type);
    foo_type->AddSuper(baseB_type);
    bar_type->AddSuper(baseA_type);
    baz_type->AddSuper(baseB_type);
    auto metavar = new TypeMetavar();

    if (i == 0) {
      EXPECT_TRUE(foo_type->ConstrainSubtypeOf(metavar));
      EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar));
      EXPECT_FALSE(baz_type->ConstrainSubtypeOf(metavar));
    } else {
      EXPECT_TRUE(foo_type->ConstrainSubtypeOf(metavar));
      EXPECT_TRUE(baz_type->ConstrainSubtypeOf(metavar));
      EXPECT_FALSE(bar_type->ConstrainSubtypeOf(metavar));
    }

    delete foo_ident;
    delete bar_ident;
    delete baz_ident;
    delete baseA_ident;
    delete baseB_ident;
    delete foo_type;
    delete bar_type;
    delete baz_type;
    delete baseA_type;
    delete baseB_type;
    delete metavar;
  }
}


TEST(TypeTest, FunctionVariable_Linear_Single1) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto foo_param = new TupleType();
  foo_param->add_element(foo_type);
  auto func_foo_to_bar = new FunctionType(false, foo_param, bar_type);

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(func_foo_to_bar->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(func_foo_to_bar));

  EXPECT_TRUE(func_foo_to_bar->IsSubtypeOf(metavar1));
  EXPECT_TRUE(func_foo_to_bar->IsSubtypeOf(metavar2));
  EXPECT_TRUE(func_foo_to_bar->IsSubtypeOf(metavar3));
  // EXPECT_FALSE(metavar1->IsSubtypeOf(func_foo_to_bar));
  // EXPECT_FALSE(metavar2->IsSubtypeOf(func_foo_to_bar));
  // EXPECT_FALSE(metavar3->IsSubtypeOf(func_foo_to_bar));

  Substitution subst;
  TypeSink allocated;
  EXPECT_TRUE(metavar1->Zonk(&subst, true, &allocated)->equals(func_foo_to_bar));
  EXPECT_TRUE(metavar2->Zonk(&subst, true, &allocated)->equals(func_foo_to_bar));
  EXPECT_TRUE(metavar3->Zonk(&subst, true, &allocated)->equals(func_foo_to_bar));

  delete foo_param;
  delete func_foo_to_bar;
  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, FunctionVariable_Linear_Single2) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto foo_param = new TupleType();
  foo_param->add_element(foo_type);
  auto func_foo_to_bar = new FunctionType(false, foo_param, bar_type);

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();

  EXPECT_TRUE(func_foo_to_bar->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(func_foo_to_bar));
  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));

  EXPECT_TRUE(func_foo_to_bar->IsSubtypeOf(metavar1));
  EXPECT_TRUE(func_foo_to_bar->IsSubtypeOf(metavar2));
  EXPECT_TRUE(func_foo_to_bar->IsSubtypeOf(metavar3));
  // EXPECT_FALSE(metavar1->IsSubtypeOf(func_foo_to_bar));
  // EXPECT_FALSE(metavar2->IsSubtypeOf(func_foo_to_bar));
  // EXPECT_FALSE(metavar3->IsSubtypeOf(func_foo_to_bar));

  Substitution subst;
  TypeSink allocated;
  EXPECT_TRUE(metavar1->Zonk(&subst, true, &allocated)->equals(func_foo_to_bar));
  EXPECT_TRUE(metavar2->Zonk(&subst, true, &allocated)->equals(func_foo_to_bar));
  EXPECT_TRUE(metavar3->Zonk(&subst, true, &allocated)->equals(func_foo_to_bar));

  delete foo_param;
  delete func_foo_to_bar;
  delete metavar1;
  delete metavar2;
  delete metavar3;
}


TEST(TypeTest, FunctionVariable_LambdaShape_Sibling) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();
  auto baz_type = types.baz_type();
  auto base1_type = types.base1_type();
  auto base2_type = types.base2_type();

  auto foo_param1 = new TupleType();
  foo_param1->add_element(foo_type);
  auto func_foo_to_bar = new FunctionType(false, foo_param1, bar_type);

  auto foo_param2 = new TupleType();
  foo_param2->add_element(foo_type);
  auto func_foo_to_baz = new FunctionType(false, foo_param2, baz_type);

  auto foo_param3 = new TupleType();
  foo_param3->add_element(foo_type);
  auto func_foo_to_base1 = new FunctionType(false, foo_param3, base1_type);

  auto foo_param4 = new TupleType();
  foo_param4->add_element(foo_type);
  auto func_foo_to_base2 = new FunctionType(false, foo_param4, base2_type);

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();
  auto metavar4 = new TypeMetavar();
  auto metavar5 = new TypeMetavar();

  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(metavar4->ConstrainSubtypeOf(metavar5));
  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(metavar5->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(func_foo_to_bar->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(func_foo_to_baz->ConstrainSubtypeOf(metavar4));
  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(func_foo_to_base1));

  Substitution subst;
  TypeSink allocated;
  EXPECT_TRUE(metavar1->Zonk(&subst, true, &allocated)->equals(func_foo_to_base2));
  EXPECT_TRUE(metavar2->Zonk(&subst, true, &allocated)->equals(func_foo_to_bar));
  EXPECT_TRUE(metavar3->Zonk(&subst, true, &allocated)->equals(func_foo_to_bar));
  EXPECT_TRUE(metavar4->Zonk(&subst, true, &allocated)->equals(func_foo_to_baz));
  EXPECT_TRUE(metavar5->Zonk(&subst, true, &allocated)->equals(func_foo_to_baz));

  delete foo_param1;
  delete func_foo_to_bar;
  delete foo_param2;
  delete func_foo_to_baz;
  delete foo_param3;
  delete func_foo_to_base2;
  delete metavar1;
  delete metavar2;
  delete metavar3;
  delete metavar4;
  delete metavar5;
}


TEST(TypeTest, FunctionVariable_PingPong1) {
  TestTypes types;
  auto bar_type = types.bar_type();

  // metavar1 -> metavar2 where metavar1 <: metavar2
  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar1_param = new TupleType();
  metavar1_param->add_element(metavar1);
  auto func_metavar1_to_metavar2 = new FunctionType(false, metavar1_param, metavar2);
  metavar1->ConstrainSubtypeOf(metavar2);

  // metavarA -> metavarB
  auto metavarA = new TypeMetavar();
  auto metavarB = new TypeMetavar();
  auto metavarA_param = new TupleType();
  metavarA_param->add_element(metavarA);
  auto func_metavarA_to_metavarB = new FunctionType(false, metavarA_param, metavarB);

  // (metavar1 -> metavar2) <: middle_metavar <: (metavarA -> metavarB)
  auto middle_metavar = new TypeMetavar();
  EXPECT_TRUE(func_metavar1_to_metavar2->ConstrainSubtypeOf(middle_metavar));
  EXPECT_TRUE(middle_metavar->ConstrainSubtypeOf(func_metavarA_to_metavarB));

  // bar_type <: metavarA
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavarA));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavarA->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavarB->Zonk(&subst, true, &allocated), bar_type);

  TypePrinter printer({.verbose = true});
  EXPECT_EQ(printer(func_metavar1_to_metavar2->Zonk(&subst, true, &allocated)), "Bar -> Bar");
  EXPECT_EQ(printer(func_metavarA_to_metavarB->Zonk(&subst, true, &allocated)), "Bar -> Bar");
  EXPECT_EQ(printer(middle_metavar->Zonk(&subst, true, &allocated)), "Bar -> Bar");

  delete metavar1;
  delete metavar2;
  delete metavar1_param;
  delete func_metavar1_to_metavar2;
  delete metavarA;
  delete metavarB;
  delete metavarA_param;
  delete func_metavarA_to_metavarB;
  delete middle_metavar;
}


TEST(TypeTest, FunctionVariable_PingPong2) {
  TestTypes types;
  auto base2_type = types.base2_type();
  auto bar_type = types.bar_type();
  auto baz_type = types.baz_type();

  // metavar1 -> metavar2 where (metavar1 | baz_type) <: metavar2
  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar1_param = new TupleType();
  metavar1_param->add_element(metavar1);
  auto func_metavar1_to_metavar2 = new FunctionType(false, metavar1_param, metavar2);
  metavar1->ConstrainSubtypeOf(metavar2);
  baz_type->ConstrainSubtypeOf(metavar2);

  // metavarA -> metavarB
  auto metavarA = new TypeMetavar();
  auto metavarB = new TypeMetavar();
  auto metavarA_param = new TupleType();
  metavarA_param->add_element(metavarA);
  auto func_metavarA_to_metavarB = new FunctionType(false, metavarA_param, metavarB);

  // (metavar1 -> metavar2) <: middle_metavar <: (metavarA -> metavarB)
  auto middle_metavar = new TypeMetavar();
  EXPECT_TRUE(func_metavar1_to_metavar2->ConstrainSubtypeOf(middle_metavar));
  EXPECT_TRUE(middle_metavar->ConstrainSubtypeOf(func_metavarA_to_metavarB));

  // bar_type <: metavarA
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavarA));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(metavar1->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavar2->Zonk(&subst, true, &allocated), base2_type);
  EXPECT_EQ(metavarA->Zonk(&subst, true, &allocated), bar_type);
  EXPECT_EQ(metavarB->Zonk(&subst, true, &allocated), base2_type);

  TypePrinter printer({.verbose = true});
  EXPECT_EQ(printer(func_metavar1_to_metavar2->Zonk(&subst, true, &allocated)), "Bar -> Base2");
  EXPECT_EQ(printer(func_metavarA_to_metavarB->Zonk(&subst, true, &allocated)), "Bar -> Base2");
  EXPECT_EQ(printer(middle_metavar->Zonk(&subst, true, &allocated)), "Bar -> Base2");

  delete metavar1;
  delete metavar2;
  delete metavar1_param;
  delete func_metavar1_to_metavar2;
  delete metavarA;
  delete metavarB;
  delete metavarA_param;
  delete func_metavarA_to_metavarB;
  delete middle_metavar;
}


TEST(TypeTest, FunctionVariable_RetSibling) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();
  auto baz_type = types.baz_type();
  auto base1_type = types.base1_type();

  // foo -> metavar1
  auto metavar1 = new TypeMetavar();
  auto foo_param1 = new TupleType();
  foo_param1->add_element(foo_type);
  auto func_foo_to_metavar1 = new FunctionType(false, foo_param1, metavar1);

  // foo -> metavar2
  auto metavar2 = new TypeMetavar();
  auto foo_param2 = new TupleType();
  foo_param2->add_element(foo_type);
  auto func_foo_to_metavar2 = new FunctionType(false, foo_param2, metavar2);

  // foo -> base1
  auto foo_param3 = new TupleType();
  foo_param3->add_element(foo_type);
  auto func_foo_to_base1 = new FunctionType(false, foo_param3, base1_type);

  // ((foo -> metavar1) | (foo -> metavar2)) <: middle_metavar <: (foo -> base1)
  auto middle_metavar = new TypeMetavar();
  EXPECT_TRUE(func_foo_to_metavar1->ConstrainSubtypeOf(middle_metavar));
  EXPECT_TRUE(func_foo_to_metavar2->ConstrainSubtypeOf(middle_metavar));
  EXPECT_TRUE(middle_metavar->ConstrainSubtypeOf(func_foo_to_base1));

  // bar <: metavar1 , baz <: metavar2
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(baz_type->ConstrainSubtypeOf(metavar2));

  Substitution subst;
  TypeSink allocated;
  TypePrinter printer({.verbose = true});
  EXPECT_EQ(printer(middle_metavar->Zonk(&subst, true, &allocated)), "Foo -> Base2");

  delete metavar1;
  delete foo_param1;
  delete func_foo_to_metavar1;
  delete metavar2;
  delete foo_param2;
  delete func_foo_to_metavar2;
  delete middle_metavar;
}


TEST(TypeTest, FunctionVariable_RetUnrelated) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  // foo -> metavar1
  auto metavar1 = new TypeMetavar();
  auto foo_param1 = new TupleType();
  foo_param1->add_element(foo_type);
  auto func_foo_to_metavar1 = new FunctionType(false, foo_param1, metavar1);

  // foo -> metavar2
  auto metavar2 = new TypeMetavar();
  auto foo_param2 = new TupleType();
  foo_param2->add_element(foo_type);
  auto func_foo_to_metavar2 = new FunctionType(false, foo_param2, metavar2);

  // ((foo -> metavar1) | (foo -> metavar2)) <: middle_metavar
  auto middle_metavar = new TypeMetavar();
  EXPECT_TRUE(func_foo_to_metavar1->ConstrainSubtypeOf(middle_metavar));
  EXPECT_TRUE(func_foo_to_metavar2->ConstrainSubtypeOf(middle_metavar));

  // foo <: metavar1 , bar <: metavar2
  EXPECT_TRUE(foo_type->ConstrainSubtypeOf(metavar1));
  EXPECT_FALSE(bar_type->ConstrainSubtypeOf(metavar2));

  delete metavar1;
  delete foo_param1;
  delete func_foo_to_metavar1;
  delete metavar2;
  delete foo_param2;
  delete func_foo_to_metavar2;
  delete middle_metavar;
}


TEST(TypeTest, FunctionVariable_ParamSibling) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto base1_type = types.base1_type();
  auto bar_type = types.bar_type();

  // bar <: (base2 & other_base)
  auto other_base_type = new NominalType(NominalType::Category::kClass, types.context()->InternIdentifier("OtherBase"));
  bar_type->AddSuper(other_base_type);

  // metavar1 -> foo
  auto metavar1 = new TypeMetavar();
  auto metavar1_param = new TupleType();
  metavar1_param->add_element(metavar1);
  auto func_metavar1_to_foo = new FunctionType(false, metavar1_param, foo_type);

  // metavar2 -> foo
  auto metavar2 = new TypeMetavar();
  auto metavar2_param = new TupleType();
  metavar2_param->add_element(metavar2);
  auto func_metavar2_to_foo = new FunctionType(false, metavar2_param, foo_type);

  // bar -> foo
  auto bar_param = new TupleType();
  bar_param->add_element(bar_type);
  auto func_bar_to_foo = new FunctionType(false, bar_param, foo_type);

  // ((metavar1 -> foo) | (metavar2 -> foo)) <: middle_metavar <: (bar -> foo)
  auto middle_metavar = new TypeMetavar();
  EXPECT_TRUE(func_metavar1_to_foo->ConstrainSubtypeOf(middle_metavar));
  EXPECT_TRUE(func_metavar2_to_foo->ConstrainSubtypeOf(middle_metavar));
  EXPECT_TRUE(middle_metavar->ConstrainSubtypeOf(func_bar_to_foo));

  // base1 <: metavar1 , other_base <: metavar2
  EXPECT_TRUE(base1_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(other_base_type->ConstrainSubtypeOf(metavar2));

  Substitution subst;
  TypeSink allocated;
  TypePrinter printer({.verbose = true});
  EXPECT_EQ(printer(middle_metavar->Zonk(&subst, true, &allocated)), "Bar -> Foo");

  delete other_base_type;
  delete metavar1;
  delete metavar1_param;
  delete func_metavar1_to_foo;
  delete metavar2;
  delete metavar2_param;
  delete func_metavar2_to_foo;
  delete bar_param;
  delete func_bar_to_foo;
  delete middle_metavar;
}


TEST(TypeTest, FunctionVariable_ParamUnrelated) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  // metavar1 -> foo
  auto metavar1 = new TypeMetavar();
  auto metavar1_param = new TupleType();
  metavar1_param->add_element(metavar1);
  auto func_metavar1_to_foo = new FunctionType(false, metavar1_param, foo_type);

  // metavar2 -> foo
  auto metavar2 = new TypeMetavar();
  auto metavar2_param = new TupleType();
  metavar2_param->add_element(metavar2);
  auto func_metavar2_to_foo = new FunctionType(false, metavar2_param, foo_type);

  // ((metavar1 -> foo) | (metavar2 -> foo)) <: middle_metavar
  auto middle_metavar = new TypeMetavar();
  EXPECT_TRUE(func_metavar1_to_foo->ConstrainSubtypeOf(middle_metavar));
  EXPECT_TRUE(func_metavar2_to_foo->ConstrainSubtypeOf(middle_metavar));

  // foo <: metavar1 , bar <: metavar2
  EXPECT_TRUE(foo_type->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(bar_type->ConstrainSubtypeOf(metavar2));

  Substitution subst;
  TypeSink allocated;
  EXPECT_EQ(middle_metavar->Zonk(&subst, true, &allocated)->kind(), Type::Kind::kError);

  delete metavar1;
  delete metavar1_param;
  delete func_metavar1_to_foo;
  delete metavar2;
  delete metavar2_param;
  delete func_metavar2_to_foo;
  delete middle_metavar;
}


TEST(TypeTest, FunctionVariable_ClosureRules) {
  TestTypes types;
  auto foo_type = types.foo_type();
  auto bar_type = types.bar_type();

  auto foo_param1 = new TupleType();
  foo_param1->add_element(foo_type);
  auto regular_foo_to_bar = new FunctionType(false, foo_param1, bar_type);

  auto foo_param2 = new TupleType();
  foo_param2->add_element(foo_type);
  auto closure_foo_to_bar = new FunctionType(true, foo_param2, bar_type);

  auto metavar1 = new TypeMetavar();
  auto metavar2 = new TypeMetavar();
  auto metavar3 = new TypeMetavar();
  auto metavar4 = new TypeMetavar();
  auto metavar5 = new TypeMetavar();
  auto metavar6 = new TypeMetavar();

  EXPECT_TRUE(regular_foo_to_bar->ConstrainSubtypeOf(metavar1));
  EXPECT_TRUE(metavar1->ConstrainSubtypeOf(closure_foo_to_bar));
  EXPECT_TRUE(closure_foo_to_bar->ConstrainSubtypeOf(metavar1));
  EXPECT_FALSE(metavar1->ConstrainSubtypeOf(regular_foo_to_bar));

  EXPECT_TRUE(regular_foo_to_bar->ConstrainSubtypeOf(metavar2));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(closure_foo_to_bar));
  EXPECT_TRUE(metavar2->ConstrainSubtypeOf(regular_foo_to_bar));
  EXPECT_FALSE(closure_foo_to_bar->ConstrainSubtypeOf(metavar2));

  EXPECT_TRUE(metavar3->ConstrainSubtypeOf(closure_foo_to_bar));
  EXPECT_TRUE(regular_foo_to_bar->ConstrainSubtypeOf(metavar3));
  EXPECT_TRUE(closure_foo_to_bar->ConstrainSubtypeOf(metavar3));
  EXPECT_FALSE(metavar3->ConstrainSubtypeOf(regular_foo_to_bar));

  EXPECT_TRUE(metavar4->ConstrainSubtypeOf(closure_foo_to_bar));
  EXPECT_TRUE(regular_foo_to_bar->ConstrainSubtypeOf(metavar4));
  EXPECT_TRUE(metavar4->ConstrainSubtypeOf(regular_foo_to_bar));
  EXPECT_FALSE(closure_foo_to_bar->ConstrainSubtypeOf(metavar4));

  EXPECT_TRUE(regular_foo_to_bar->ConstrainSubtypeOf(metavar5));
  EXPECT_TRUE(closure_foo_to_bar->ConstrainSubtypeOf(metavar5));

  EXPECT_TRUE(metavar6->ConstrainSubtypeOf(closure_foo_to_bar));
  EXPECT_TRUE(metavar6->ConstrainSubtypeOf(regular_foo_to_bar));

  delete foo_param1;
  delete regular_foo_to_bar;
  delete foo_param2;
  delete closure_foo_to_bar;
  delete metavar1;
  delete metavar2;
  delete metavar3;
  delete metavar4;
  delete metavar5;
  delete metavar6;
}


TEST(TypeTest, ExtractField_FromNominal) {
  TestTypes types;
  auto bar_type = types.bar_type();
  auto ident_age = types.context()->InternIdentifier("age");

  // Animal { age: bar }
  auto animal_type = new NominalType(NominalType::Category::kClass, types.context()->InternIdentifier("Animal"));
  animal_type->AddField(ident_age, bar_type);

  // Dog <: Animal
  auto dog_type = new NominalType(NominalType::Category::kClass, types.context()->InternIdentifier("Dog"));
  dog_type->AddSuper(animal_type);

  // has member
  auto extvar = new TypeMetavar();
  auto memreq = new MemberRequirement(ident_age, extvar);

  // constrain member
  EXPECT_TRUE(dog_type->ConstrainSubtypeOf(memreq));
  EXPECT_TRUE(extvar->IsSubtypeOf(bar_type));
  EXPECT_TRUE(bar_type->IsSubtypeOf(extvar));

  delete animal_type;
  delete dog_type;
  delete extvar;
  delete memreq;
}


TEST(TypeTest, ExtractField_FromTyvar) {
  TestTypes types;
  auto bar_type = types.bar_type();
  auto ident_age = types.context()->InternIdentifier("age");

  // Animal { age: bar }
  auto animal_type = new NominalType(NominalType::Category::kClass, types.context()->InternIdentifier("Animal"));
  animal_type->AddField(ident_age, bar_type);

  // Dog <: Animal
  auto dog_type = new NominalType(NominalType::Category::kClass, types.context()->InternIdentifier("Dog"));
  dog_type->AddSuper(animal_type);

  // Dog <: tyvar
  auto tyvar = new TypeVariable(0);
  EXPECT_TRUE(dog_type->ConstrainSubtypeOf(tyvar));

  // has member
  auto extvar = new TypeMetavar();
  auto memreq = new MemberRequirement(ident_age, extvar);

  // constrain member
  EXPECT_TRUE(tyvar->ConstrainSubtypeOf(memreq));
  EXPECT_TRUE(extvar->IsSubtypeOf(bar_type));
  EXPECT_TRUE(bar_type->IsSubtypeOf(extvar));

  delete animal_type;
  delete dog_type;
  delete tyvar;
  delete extvar;
  delete memreq;
}


TEST(TypeTest, ExtractField_FakeSubtyping) {
  TestTypes types;
  auto bar_type = types.bar_type();
  auto ident_name = types.context()->InternIdentifier("name");

  // Animal
  auto animal_type = new NominalType(NominalType::Category::kClass, types.context()->InternIdentifier("Animal"));

  // Dog { name: bar } <: Animal
  auto dog_type = new NominalType(NominalType::Category::kClass, types.context()->InternIdentifier("Dog"));
  dog_type->AddSuper(animal_type);
  dog_type->AddField(ident_name, bar_type);

  // Cat { name: bar } <: Animal
  auto cat_type = new NominalType(NominalType::Category::kClass, types.context()->InternIdentifier("Cat"));
  cat_type->AddSuper(animal_type);
  cat_type->AddField(ident_name, bar_type);

  // (Dog | Cat) <: metavar
  auto metavar = new TypeMetavar();
  EXPECT_TRUE(dog_type->ConstrainSubtypeOf(metavar));
  EXPECT_TRUE(cat_type->ConstrainSubtypeOf(metavar));

  // has member
  auto extvar = new TypeMetavar();
  auto memreq = new MemberRequirement(ident_name, extvar);

  // constrain member
  EXPECT_FALSE(metavar->ConstrainSubtypeOf(memreq));

  delete animal_type;
  delete dog_type;
  delete cat_type;
  delete metavar;
  delete extvar;
  delete memreq;
}


}  // namespace xylo
