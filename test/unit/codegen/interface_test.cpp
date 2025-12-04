
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
      def get_value(): int => value
    }

    class Bar : ValueGetter {
      def get_value(): int => 0
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
      def get_value(): int => value
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
      def get_value(): int => value
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
      def get_value(): int => value
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
      def get_value(): int => value
      def set_value(v: int): unit { this.value = v }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(InterfaceTest, ImplementsUsingEmbedding) {
  auto source = R"(
    def main() {
      let foo = new Foo{ Bar: { value: 0 } }
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
      embed Bar
    }

    class Bar {
      value: int
      def get_value(): int => value
      def set_value(v: int): unit { this.value = v }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(InferencerTest, MultipleImplementsUsingEmbedding) {
  auto source = R"(
    def main() {
      let foo = new Foo{ Bar: { value: 0 } }
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
      embed Bar
    }

    class Bar {
      value: int
      def get_value(): int => value
      def set_value(v: int): unit { this.value = v }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 84);
}


}  // namespace xylo
