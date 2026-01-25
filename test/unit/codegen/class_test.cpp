
#include <gtest/gtest.h>

#include "./compiler_common.h"

namespace xylo {


TEST(ClassTest, EmptyClass) {
  auto source = R"(
    class Empty {
    }

    def main() {
      let e = new Empty{}
      return 42
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, FieldRead) {
  auto source = R"(
    class Point {
      x: int
      y: int
    }

    def main() {
      let pt = new Point{ x: 10, y: 20 }
      return pt.x + pt.y
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 30);
}


TEST(ClassTest, FieldWrite) {
  auto source = R"(
    class Counter {
      count: int
    }

    def main() {
      let counter = new Counter{ count: 0 }
      counter.count = 10
      return counter.count
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 10);
}


TEST(ClassTest, Getter_Implicit) {
  auto source = R"(
    class Counter {
      count: int

      def get_count() {
        return count
      }
    }

    def main() {
      let counter = new Counter{ count: 10 }
      return counter.get_count()
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 10);
}


TEST(ClassTest, Getter_This) {
  auto source = R"(
    class Counter {
      count: int

      def get_count() {
        return this.count
      }
    }

    def main() {
      let counter = new Counter{ count: 10 }
      return counter.get_count()
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 10);
}


TEST(ClassTest, Setter_Implicit) {
  auto source = R"(
    class Counter {
      count: int

      def set_count(value) {
        count = value
      }
    }

    def main() {
      let counter = new Counter{ count: 0 }
      counter.set_count(10)
      return counter.count
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 10);
}


TEST(ClassTest, Setter_This) {
  auto source = R"(
    class Counter {
      count: int

      def set_count(value) {
        this.count = value
      }
    }

    def main() {
      let counter = new Counter{ count: 0 }
      counter.set_count(10)
      return counter.count
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 10);
}


TEST(ClassTest, MethodCallInMethod_Implicit) {
  auto source = R"(
    class Counter {
      count: int

      def increment() {
        count = get_count() + 1
      }

      def get_count() {
        return count
      }
    }

    def main() {
      let counter = new Counter{ count: 0 }
      counter.increment()
      counter.increment()
      return counter.get_count()
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 2);
}


TEST(ClassTest, MethodCallInMethod_This) {
  auto source = R"(
    class Counter {
      count: int

      def increment() {
        this.count = this.get_count() + 1
      }

      def get_count() {
        return this.count
      }
    }

    def main() {
      let counter = new Counter{ count: 0 }
      counter.increment()
      counter.increment()
      return counter.get_count()
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 2);
}


TEST(ClassTest, NestedFieldAccess_Implicit) {
  auto source = R"(
    class Counter {
      count: int

      def get_count() {
        return inner()

        def inner() {
          return count
        }
      }
    }

    def main() {
      let counter = new Counter{ count: 10 }
      return counter.get_count()
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 10);
}


TEST(ClassTest, NestedFieldAccess_This) {
  auto source = R"(
    class Counter {
      count: int

      def get_count() {
        return middle()

        def middle() {
          return inner()

          def inner() {
            return this.count
          }
        }
      }
    }

    def main() {
      let counter = new Counter{ count: 10 }
      return counter.get_count()
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 10);
}


TEST(ClassTest, PolymorphicMethod) {
  auto source = R"(
    class Foo {
      def id(a) {
        return a
      }
    }

    def main() {
      let foo = new Foo{}
      return foo.id(add)(foo.id(20), foo.id(22))
    }

    def add(a, b) {
      return a + b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, FieldReadInPolymorphicFunc) {
  auto source = R"(
    class BoxA {
      value: int
    }

    class BoxB {
      value: int
    }

    def main() {
      let box1 = new BoxA{ value: 32 }
      let box2 = new BoxB{ value: 10 }
      return get_value(box1) + get_value(box2)
    }

    def get_value(b) {
      return b.value
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, FieldWriteInPolymorphicFunc) {
  auto source = R"(
    class BoxA {
      value: int
    }

    class BoxB {
      value: int
    }

    def main() {
      let box1 = new BoxA{ value: 0 }
      let box2 = new BoxB{ value: 0 }
      set_value(box1, 32)
      set_value(box2, 10)
      return box1.value + box2.value
    }

    def set_value(b, v) {
      b.value = v
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, MethodCallInPolymorphicFunc) {
  auto source = R"(
    class BoxA {
      value: int

      def get_value() {
        return value
      }
    }

    class BoxB {
      value: int

      def get_value() {
        return value
      }
    }

    def main() {
      let box1 = new BoxA{ value: 32 }
      let box2 = new BoxB{ value: 10 }
      return get_value(box1) + get_value(box2)
    }

    def get_value(b) {
      return b.get_value()
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, PolymorphicMethodCallInPolymorphicFunc) {
  auto source = R"(
    class Doubler {
      def transform(x) {
        return x * 2
      }
    }

    class Identity {
      def transform(x) {
        return x
      }
    }

    def main() {
      let doubler = new Doubler{}
      let identity = new Identity{}
      let value1 = transform(doubler, 21)
      let value2 = transform(identity, true)
      let check = value1 == 42 && value2
      return check ? 1 : 0
    }

    def transform(f, x) {
      return f.transform(x)
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(ClassTest, RecursiveMethod) {
  auto source = R"(
    class Factorial {
      def factorial(n: int): int {
        return n == 0 ? 1 : n * this.factorial(n - 1)
      }
    }

    def main() {
      let fact = new Factorial{}
      return fact.factorial(5)
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 120);
}


TEST(ClassTest, MutualRecursionMethod) {
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
      return (a ? 1 : 0) + (b ? 1 : 0)
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 2);
}


TEST(ClassTest, MethodConstraintInNestedFunc) {
  auto source = R"(
    class Foo {
      def foo() {
        return 42
      }
    }

    def main() {
      return outer(new Foo{})
    }

    def outer(v1) {
      return inner(new Foo{})

      def inner(v2) {
        var a = true ? v1 : v2
        return a.foo()
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, ClassInFunction) {
  auto source = R"(
    def main() {
      let foo = new Foo{ value: 42 }
      return foo.get_value()

      class Foo {
        value: int
        def get_value() => value
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, ClassInFunction_Escape) {
  auto source = R"(
    def main() {
      return create_foo(42).get_value()
    }

    def create_foo(v: int) {
      return new Foo{ value: v }

      class Foo {
        value: int
        def get_value() => value
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, ClassInFunction_DeepEscape) {
  auto source = R"(
    def main() {
      return create_foo(42).get_value()
    }

    def create_foo(v: int) {
      return inner_create(10)

      def inner_create(a) {
        return new Foo{ value: v }

        class Foo {
          value: int
          def get_value() => value + a
        }
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 52);
}


TEST(ClassTest, ClassInFunction_Polymorphic1) {
  auto source = R"(
    def main() {
      let foo1 = create_foo(new Bar{})
      let foo2 = create_foo(new Baz{})
      let check = foo1.get_object().get() == 1 && foo2.get_object().get() == 2
      return check ? 1 : 0
    }

    def create_foo(obj) {
      return new Foo{}

      class Foo {
        def get_object() => obj
      }
    }

    class Bar { def get() => 1 }
    class Baz { def get() => 2 }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(ClassTest, ClassInFunction_Polymorphic2) {
  auto source = R"(
    def main() {
      let value1 = bridge(new Bar{})
      let value2 = bridge(new Baz{})
      let check = value1 == 1 && value2 == 2
      return check ? 1 : 0
    }

    def bridge(v) {
      return create_foo(v).get_object().get()
    }

    def create_foo(obj) {
      return new Foo{}

      class Foo {
        def get_object() => obj
      }
    }

    class Bar { def get() => 1 }
    class Baz { def get() => 2 }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(ClassTest, ClassInFunction_ParamSlide) {
  auto source = R"(
    def main() {
      let foo = outer(42)
      return foo.get_value()
    }

    def outer(v) {
      return inner(v)

      def inner(v) {
        return new Foo{}

        class Foo {
          def get_value() => v
        }
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, ClassInFunction_WithPolymorphicMethod) {
  auto source = R"(
    def main() {
      let foo = outer(new Bar{})
      let obj1 = foo.get_object(true, new Bar{})
      let obj2 = foo.get_object(true, new Baz{})
      let check = obj1.get_value() == 1 && obj2.get_value() == 4 && obj1.bar() == true
      return check ? 1 : 0
    }

    def outer(default_obj) {
      return inner()

      def inner() {
        return new Foo{}

        class Foo {
          def get_object(cond, obj) => cond ? obj : default_obj
        }
      }
    }

    interface ValueGetter {
      def get_value(): int
    }
    class Bar : ValueGetter { def get_value(): int => 1; def bar() => true }
    class Baz : ValueGetter { def get_value(): int => 4 }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(ClassTest, ClassInLambda) {
  auto source = R"(
    def main() {
      let result = outer(5.0).compute()
      return result == 50.0 ? 1 : 0
    }

    def outer(x) {
      let foo = new Foo{value: x}

      let create_bar = fn(y) {
        return new Bar{value: y}
        class Bar {
          value: float
          def compute() => foo.mul(value)
        }
      }

      return create_bar(10.0)
    }

    class Foo {
      value: float
      def mul(a) => value * a
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(ClassTest, CLassInClass) {
  auto source = R"(
    class Outer {
      value: int

      def create_inner() => new Inner{}

      class Inner {
        def get_value() => value
      }
    }

    def main() {
      let outer = new Outer{ value: 42 }
      let inner = outer.create_inner()
      return inner.get_value()
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, ClassInMethod) {
  auto source = R"(
    def main() {
      let foo_int = outer(10)
      let bar_int = foo_int.get_bar(20)
      let val_int_a = bar_int.get(true)
      let val_int_b = bar_int.get(false)

      let foo_float = outer(1.5)
      let bar_float = foo_float.get_bar(2.5)
      let val_float_a = bar_float.get(true)
      let val_float_b = bar_float.get(false)

      let check = (val_int_a == 20) && (val_int_b == 40) && (val_float_a == 3.0) && (val_float_b == 5.0)
      return check ? 1 : 0
    }

    def outer(a) {
      return new Foo{}

      class Foo {
        def get_bar(b) {
          return new Bar{}

          class Bar {
            def get(c) {
              return (c ? a : b) * 2
            }
          }
        }
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(ClassTest, Embedding_PromotedFieldRead) {
  auto source = R"(
    class Bar {
      bar_field: int
    }

    class Foo {
      foo_field: int
      embed bar: Bar
    }

    def main() {
      let foo = new Foo {
        foo_field: 12,
        bar: { bar_field: 30 },
      }
      return foo.foo_field + foo.bar_field
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, Embedding_ExplicitFieldRead) {
  auto source = R"(
    class Bar {
      bar_field: int
    }

    class Foo {
      foo_field: int
      embed bar: Bar
    }

    def main() {
      let foo = new Foo {
        foo_field: 12,
        bar: { bar_field: 30 },
      }
      return foo.foo_field + foo.bar.bar_field
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, Embedding_PromotedFieldWrite) {
  auto source = R"(
    class Bar {
      bar_field: int
    }

    class Foo {
      foo_field: int
      embed bar: Bar
    }

    def main() {
      let foo = new Foo {
        foo_field: 0,
        bar: { bar_field: 0 },
      }
      foo.foo_field = 12
      foo.bar_field = 30
      return foo.foo_field + foo.bar_field
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, Embedding_ExplicitFieldWrite) {
  auto source = R"(
    class Bar {
      bar_field: int
    }

    class Foo {
      foo_field: int
      embed bar: Bar
    }

    def main() {
      let foo = new Foo {
        foo_field: 0,
        bar: { bar_field: 0 },
      }
      foo.foo_field = 12
      foo.bar.bar_field = 30
      return foo.foo_field + foo.bar.bar_field
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, Embedding_PromotedMethod) {
  auto source = R"(
    class Bar {
      bar_field: int
      def get_bar_field() {
        return bar_field
      }
    }

    class Foo {
      foo_field: int
      embed bar: Bar

      def get_foo_field() {
        return foo_field
      }
    }

    def main() {
      let foo = new Foo {
        foo_field: 12,
        bar: { bar_field: 30 },
      }
      return foo.get_foo_field() + foo.get_bar_field()
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, Embedding_ExplicitMethod) {
  auto source = R"(
    class Bar {
      bar_field: int
      def get_bar_field() {
        return bar_field
      }
    }

    class Foo {
      foo_field: int
      embed bar: Bar

      def get_foo_field() {
        return foo_field
      }
    }

    def main() {
      let foo = new Foo {
        foo_field: 12,
        bar: { bar_field: 30 },
      }
      return foo.get_foo_field() + foo.bar.get_bar_field()
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, Embedding_Priority) {
  auto source = R"(
    class Bar {
      value: int
    }

    class Foo {
      value: int
      embed bar: Bar
    }

    def main() {
      let foo = new Foo {
        value: 10,
        bar: { value: 30 },
      }
      return foo.value
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 10);
}


TEST(ClassTest, Embedding_DifferentScope) {
  auto source = R"(
    def main() {
      let foo = outer(42)
      return foo.get_value()
    }

    def outer(v) {
      return inner(v)

      def inner(v) {
        return new Foo{ bar: { value: v } }

        class Foo {
          embed bar: Bar
        }
      }

      class Bar {
        value: int
        def get_value() => value
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(ClassTest, Embedding_ClosureClass) {
  auto source = R"(
    def main() {
      let foo = outer(42)
      return foo.get_value()
    }

    def outer(v) {
      return inner(v)

      def inner(v) {
        return new Foo{ bar: {} }

        class Foo {
          embed bar: Bar
        }
      }

      class Bar {
        def get_value() => v
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


}  // namespace xylo
