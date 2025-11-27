
#include <gtest/gtest.h>

#include "./compiler_common.h"

namespace xylo {


TEST(ValueTest, LetVariable) {
  auto source = R"(
    def main() {
      let a = 10
      let b = 20
      return a + b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 30);
}


TEST(ValueTest, VarVariable) {
  auto source = R"(
    def main() {
      var a = 1
      a = a + 0.1
      return a > 1.0 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(ValueTest, NestedBlocks) {
  auto source = R"(
    def main() {
      let x = 5
      {
        let y = 10
        {
          let z = 15
          return x + y + z
        }
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 30);
}


TEST(ValueTest, MergeNumerics) {
  auto source = R"(
    def main() {
      let a = true ? 5 : 5.5
      return a <= 5.0 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(ValueTest, MergeIncompatibleButUnused) {
  auto source = R"(
    def main() {
      merge(new Foo{}, new Bar{})
      return 0
    }

    class Foo {}
    class Bar {}

    def merge(a, b) {
      var x = true ? a : b
      id(x)
    }

    def id(a) => a
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(ValueTest, UnitToLet) {
  auto source = R"(
    def main() {
      let a = foo()
      a
      return 0
    }

    def foo() {}
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(ValueTest, UnitToVar) {
  auto source = R"(
    def main() {
      var a = foo()
      a
      return 0
    }

    def foo() {}
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(ValueTest, UnitToIdentFunction) {
  auto source = R"(
    def main() {
      id(foo())
      return 0
    }

    def id(a) => a
    def foo() {}
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(ValueTest, UnionToApplyFunction) {
  auto source = R"(
    def main() {
      var a = apply(foo, 10)
      return 0
    }

    def apply(f, x) => f(x)
    def foo(x) {}
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


}  // namespace xylo
