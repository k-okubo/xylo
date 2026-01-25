
#include <gtest/gtest.h>

#include "./compiler_common.h"

namespace xylo {


TEST(GenericsTest, GenericClass_UnusedTypeParam) {
  auto source = R"(
    def main() {
      let box_int = new Box[bool]{ value: 30 }
      return box_int.get_value()
    }

    class Box[T] {
      value: int
      def get_value() => value
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 30);
}


TEST(GenericsTest, GenericClass_ParamedField1) {
  auto source = R"(
    def main() {
      let box_int = new Box[int]{ value: 30 }
      let box_bool = new Box[bool]{ value: true }
      let check = box_int.get_value() == 30 && box_bool.get_value() == true
      return check ? 1 : 0
    }

    class Box[T] {
      value: T
      def get_value() => value
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(GenericsTest, GenericClass_ParamedField2) {
  auto source = R"(
    def main() {
      let foo = new Foo[int]{ bar: new Bar[int]{ value: 42 } }
      return foo.bar.get_value() == 42 ? 1 : 0
    }

    class Foo[T] {
      bar: Bar[T]
    }

    class Bar[T] {
      value: T
      def get_value() => value
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(GenericsTest, GenericClass_ParamedMethod) {
  auto source = R"(
    def main() {
      let foo_int = new Foo[int]{}
      let foo_bool = new Foo[bool]{}
      let check = foo_int.id(30) == 30 && foo_bool.id(true) == true
      return check ? 1 : 0
    }

    class Foo[T] {
      def id(v: T): T {
        return v
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(GenericsTest, GenericClass_ParamedEmbedding) {
  auto source = R"(
    def main() {
      let foo_int = new Foo[int]{
        foo_field: 10,
        bar: { bar_field: 20 },
      }
      let foo_bool = new Foo[bool]{
        foo_field: true,
        bar: { bar_field: false },
      }

      let check = foo_int.bar_field == 20 && foo_bool.bar_field == false
      return check ? 1 : 0
    }

    class Foo[T] {
      foo_field: T
      embed bar: Bar[T]
    }

    class Bar[T] {
      bar_field: T
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(GenericsTest, GenericClass_OuterParamedField1) {
  auto source = R"(
    def main() {
      let outer = new Outer[float]{}
      let inner = outer.create_inner(42, true)
      let check = inner.first == 42.0 && inner.second == true
      return check ? 1 : 0
    }

    class Outer[T] {
      def create_inner(first: T, second: bool) => new Inner[bool]{ first: first, second: second }

      class Inner[S] {
        first: T
        second: S
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(GenericsTest, GenericClass_OuterParamedField2) {
  auto source = R"(
    def main() {
      let outer = new Outer[int]{}
      let inner = outer.create_inner(30)
      return inner.bar.get_value() == 30 ? 1 : 0
    }

    class Outer[T] {
      def create_inner(value: T) => new Inner{ bar: new Bar[T]{ value: value } }

      class Inner {
        bar: Bar[T]
      }
    }

    class Bar[T] {
      value: T
      def get_value() => value
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(GenericsTest, GenericClass_OuterParamedMethod) {
  auto source = R"(
    def main() {
      let outer = new Outer[float]{}
      let inner = outer.create_inner()
      let check = inner.id(10.0) == 10.0
      return check ? 1 : 0
    }

    class Outer[T] {
      def create_inner() => new Inner{}

      class Inner {
        def id(v: T): T {
          return v
        }
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(GenericsTest, GenericClass_OuterParamedEmbedding) {
  auto source = R"(
    def main() {
      let outer = new Outer[int]{}
      let inner = outer.create_inner(30)
      let check = inner.bar.bar_field == 30
      return check ? 1 : 0
    }

    class Outer[T] {
      def create_inner(value: T) => new Inner{ bar: { bar_field: value } }

      class Inner {
        embed bar: Bar[T]
      }
    }

    class Bar[T] {
      bar_field: T
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(GenericsTest, GenericInterface_Basic) {
  auto source = R"(
    def main() {
      let foo = new Foo{value: 42}
      let bar = new Bar{value: true}
      let check = bridge_int(foo) == 42 && bridge_bool(bar) == true
      return check ? 1 : 0
    }

    def bridge_int(x: Box[int]) {
      return x.get_value()
    }

    def bridge_bool(x: Box[bool]) {
      return x.get_value()
    }

    class Foo : Box[int] {
      value: int
      def get_value() => value
    }

    class Bar : Box[bool] {
      value: bool
      def get_value() => value
    }

    interface Box[T] {
      def get_value(): T
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(GenericsTest, GenericClass_ParamedInheritance) {
  auto source = R"(
    def main() {
      let foo_int = new Foo[int]{value: 42}
      let foo_bool = new Foo[bool]{value: true}
      let check = bridge_int(foo_int) == 42 && bridge_bool(foo_bool) == true
      return check ? 1 : 0
    }

    def bridge_int(x: Box[int]) {
      return x.get_value()
    }

    def bridge_bool(x: Box[bool]) {
      return x.get_value()
    }

    class Foo[T] : Box[T] {
      value: T
      def get_value() => value
    }

    interface Box[T] {
      def get_value(): T
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(GenericsTest, GenericClass_OuterParamedInheritance) {
  auto source = R"(
    def main() {
      let outer_int = new Outer[int]{}
      let outer_bool = new Outer[bool]{}
      let inner_int = outer_int.create_inner(42)
      let inner_bool = outer_bool.create_inner(true)
      let check = bridge_int(inner_int) == 42 && bridge_bool(inner_bool) == true
      return check ? 1 : 0
    }

    def bridge_int(x: Box[int]) {
      return x.get_value()
    }

    def bridge_bool(x: Box[bool]) {
      return x.get_value()
    }

    class Outer[T] {
      def create_inner(value: T) => new Inner{ value: value }

      class Inner : Box[T] {
        value: T
        def get_value() => value
      }
    }

    interface Box[T] {
      def get_value(): T
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


}  // namespace xylo
