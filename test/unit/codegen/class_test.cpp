
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


}  // namespace xylo
