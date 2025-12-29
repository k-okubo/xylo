
#include <gtest/gtest.h>

#include "./compiler_common.h"

namespace xylo {


TEST(GenericsTest, GenericClass_Basic) {
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


TEST(GenericTest, GenericInterface_Basic) {
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


#if 0
TEST(GenericTest, GenericInterface_TyvarParamed) {
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
#endif


}  // namespace xylo
