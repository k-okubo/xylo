
#include <gtest/gtest.h>

#include "./compiler_common.h"

namespace xylo {


TEST(InterfaceTest, Basic) {
  auto source = R"(
    def main() {
      let x = true ? new Foo{ value: 42 } : new Bar{}
      return x.get_value()
    }

    interface ValueGetter {
      def get_value(): int
    }

    class Foo : ValueGetter {
      value: int
      def get_value() => value
    }

    class Bar : ValueGetter {
      def get_value() => 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(InterfaceTest, ClassToInterface) {
  auto source = R"(
    def main() {
      let foo = new Foo{ value: 42 }
      return get(foo)
    }

    def get(a: ValueGetter) {
      return a.get_value()
    }

    interface ValueGetter {
      def get_value(): int
    }

    class Foo : ValueGetter {
      value: int
      def get_value() => value
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(InterfaceTest, InterfaceUpcast) {
  auto source = R"(
    def main() {
      let foo = new Foo{ value: 42 }
      return middle_get(foo)
    }

    def middle_get(a: MiddleGetter) {
      return get(a)
    }

    def get(a: ValueGetter) {
      return a.get_value()
    }

    interface ValueGetter {
      def get_value(): int
    }

    interface MiddleGetter : ValueGetter {}

    class Foo : MiddleGetter {
      value: int
      def get_value() => value
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(InterfaceTest, SupertypesMethodCall) {
  auto source = R"(
    def main() {
      let foo = new Foo{ value: 42 }
      return middle_get(foo)
    }

    def middle_get(a: MiddleGetter) {
      return a.get_value()
    }

    interface ValueGetter {
      def get_value(): int
    }

    interface MiddleGetter : ValueGetter {}

    class Foo : MiddleGetter {
      value: int
      def get_value() => value
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(InterfaceTest, MultipleInterfaces) {
  auto source = R"(
    def main() {
      let foo = new Foo{ value: 0 }
      set_value(foo, 42)
      return get_value(foo)
    }

    def get_value(a: ValueGetter) {
      return a.get_value()
    }

    def set_value(a: ValueSetter, v) {
      a.set_value(v)
    }

    interface ValueGetter {
      def get_value(): int
    }

    interface ValueSetter {
      def set_value(v: int): unit
    }

    class Foo : ValueGetter, ValueSetter {
      value: int
      def get_value() => value
      def set_value(v) { this.value = v }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(InterfaceTest, DifferentScope) {
  auto source = R"(
    def main() {
      return outer(true, 42, 0).get_value()
    }

    def outer(c, a, b) {
      return c ? create_foo() : create_bar()

      def create_foo() {
        return new Foo{}
        class Foo : ValueGetter {
          def get_value() => a
        }
      }

      def create_bar() {
        return new Bar{}
        class Bar : ValueGetter {
          def get_value() => b
        }
      }

      interface ValueGetter {
        def get_value(): int
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(InterfaceTest, ImplementsWithPolymorphicMethod) {
  auto source = R"(
    def main() {
      let foo = new Foo{}
      let check = transform(foo, 1) == 2
      return check ? 1 : 0
    }

    def transform(t: Transformer, v) {
      return t.transform(v)
    }

    interface Transformer {
      def transform(v: int): int
    }

    class Foo : Transformer {
      def transform(v) => v * 2
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(InterfaceTest, ImplementsWithBridge) {
  auto source = R"(
    def main() {
      let foo = new Foo{}
      let check = transform(foo, 1) == 2.0
      return check ? 1 : 0
    }

    def transform(t: Transformer, v) {
      return t.transform(v)
    }

    interface Transformer {
      def transform(v: int): float
    }

    class Foo : Transformer {
      def transform(v: float) => v * 2.0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(InterfaceTest, ImplementsMultipleInterfaces) {
  auto source = R"(
    def main() {
      let foo = new Foo{}
      let int_result = transform_int(foo, 42)
      let float_result = transform_float(foo, 42.0)
      let check = int_result == 84 && float_result == 84.0
      return check ? 1 : 0
    }

    def transform_int(t: IntTransformer, v) {
      return t.transform(v)
    }

    def transform_float(t: FloatTransformer, v) {
      return t.transform(v)
    }

    interface IntTransformer {
      def transform(v: int): int
    }

    interface FloatTransformer {
      def transform(v: float): float
    }

    class Foo : IntTransformer, FloatTransformer {
      def transform(v) => v * 2
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(InterfaceTest, ImplementsUsingEmbedding) {
  auto source = R"(
    def main() {
      let foo = new Foo{ bar: { value: 0 } }
      set_value(foo, 42)
      return get_value(foo)
    }

    def get_value(a: ValueGetter) {
      return a.get_value()
    }

    def set_value(a: ValueSetter, v) {
      a.set_value(v)
    }

    interface ValueGetter {
      def get_value(): int
    }

    interface ValueSetter {
      def set_value(v: int): unit
    }

    class Foo : ValueGetter, ValueSetter {
      embed bar: Bar
    }

    class Bar {
      value: int
      def get_value() => value
      def set_value(v) { this.value = v }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(Interface, MultipleImplementsUsingEmbedding) {
  auto source = R"(
    def main() {
      let foo = new Foo{ bar: { value: 0 } }
      set_value(foo, 42)
      return get_value1(foo) + get_value2(foo)
    }

    def get_value1(a: ValueGetter) {
      return a.get_value()
    }

    def get_value2(a: ValueContainer) {
      return a.get_value()
    }

    def set_value(a: ValueContainer, v) {
      a.set_value(v)
    }

    interface ValueGetter {
      def get_value(): int
    }

    interface ValueContainer {
      def get_value(): int
      def set_value(v: int): unit
    }

    class Foo : ValueGetter, ValueContainer {
      embed bar: Bar
    }

    class Bar {
      value: int
      def get_value() => value
      def set_value(v) { this.value = v }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 84);
}


}  // namespace xylo
