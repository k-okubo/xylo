
#include <gtest/gtest.h>

#include "./compiler_common.h"

namespace xylo {


TEST(OperatorTest, AddInteger) {
  auto source = R"(
    def main() {
      return 1 + 2
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 3);
}


TEST(OperatorTest, SubtractInteger) {
  auto source = R"(
    def main() {
      return 5 - 3
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 2);
}


TEST(OperatorTest, MultiplyInteger) {
  auto source = R"(
    def main() {
      return 4 * 6
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 24);
}


TEST(OperatorTest, DivideInteger) {
  auto source = R"(
    def main() {
      return 7 / 3
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 2);
}


TEST(OperatorTest, RemainderInteger) {
  auto source = R"(
    def main() {
      return 7 % 3
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, AddFloat) {
  auto source = R"(
    def main() {
      return 1.5 + 2.5 == 4.0 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, SubstractFloat) {
  auto source = R"(
    def main() {
      return 5.5 - 3.0 == 2.5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, MultiplyFloat) {
  auto source = R"(
    def main() {
      return 4.0 * 2.5 == 10.0 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, DivideFloat) {
  auto source = R"(
    def main() {
      return 7.5 / 2.5 == 3.0 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, RemainderFloat) {
  auto source = R"(
    def main() {
      return 7.5 % 3.0 == 1.5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, AddMixed) {
  auto source = R"(
    def main() {
      let a = 10
      let b = 2.5
      return a + b == 12.5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, SubtractMixed) {
  auto source = R"(
    def main() {
      let a = 10
      let b = 2.5
      return a - b == 7.5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, MultiplyMixed) {
  auto source = R"(
    def main() {
      let a = 3
      let b = 2.5
      return a * b == 7.5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, DivideMixed) {
  auto source = R"(
    def main() {
      let a = 10
      let b = 2.5
      return a / b == 4.0 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, RemainderMixed) {
  auto source = R"(
    def main() {
      let a = 8
      let b = 2.5
      return a % b == 0.5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, PositiveInteger) {
  auto source = R"(
    def main() {
      let a = -42
      return +a
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, -42);
}


TEST(OperatorTest, NegativeInteger) {
  auto source = R"(
    def main() {
      let a = 42
      return -a
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, -42);
}


TEST(OperatorTest, PositiveFloat) {
  auto source = R"(
    def main() {
      let a = -3.5
      return +a == -3.5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, NegativeFloat) {
  auto source = R"(
    def main() {
      let a = 3.5
      return -a == -3.5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, BitAnd) {
  auto source = R"(
    def main() {
      return 6 & 3
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 2);
}


TEST(OperatorTest, BitOr) {
  auto source = R"(
    def main() {
      return 6 | 3
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 7);
}


TEST(OperatorTest, BitXor) {
  auto source = R"(
    def main() {
      return 6 ^ 3
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 5);
}


TEST(OperatorTest, BitNot) {
  auto source = R"(
    def main() {
      return ~6
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, -7);
}


TEST(OperatorTest, LeftShift) {
  auto source = R"(
    def main() {
      return 3 << 2
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 12);
}


TEST(OperatorTest, RightShift) {
  auto source = R"(
    def main() {
      return 12 >> 2
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 3);
}


TEST(OperatorTest, LogicalAndTrueTrue) {
  auto source = R"(
    def main() {
      return true && true ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, LogicalAndTrueFalse) {
  auto source = R"(
    def main() {
      return true && false ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(OperatorTest, LogicalAndFalseTrue) {
  auto source = R"(
    def main() {
      return false && true ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(OperatorTest, LogicalAndFalseFalse) {
  auto source = R"(
    def main() {
      return false && false ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(OperatorTest, LogicalOrTrueTrue) {
  auto source = R"(
    def main() {
      return true || true ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, LogicalOrTrueFalse) {
  auto source = R"(
    def main() {
      return true || false ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, LogicalOrFalseTrue) {
  auto source = R"(
    def main() {
      return false || true ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, LogicalOrFalseFalse) {
  auto source = R"(
    def main() {
      return false || false ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(OperatorTest, LogicalNotTrue) {
  auto source = R"(
    def main() {
      return !true ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(OperatorTest, LogicalNotFalse) {
  auto source = R"(
    def main() {
      return !false ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, LessThanInteger) {
  auto source = R"(
    def main() {
      return 3 < 5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, LessThanEqualInteger) {
  auto source = R"(
    def main() {
      return 5 <= 5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, LessThanEqualInteger2) {
  auto source = R"(
    def main() {
      return 3 <= 5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, GreaterThanInteger) {
  auto source = R"(
    def main() {
      return 7 > 5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, GreaterThanEqualInteger) {
  auto source = R"(
    def main() {
      return 5 >= 5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, GreaterThanEqualInteger2) {
  auto source = R"(
    def main() {
      return 7 >= 5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, LessThanFloat) {
  auto source = R"(
    def main() {
      return 3.5 < 5.0 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, LessThanEqualFloat) {
  auto source = R"(
    def main() {
      return 5.0 <= 5.0 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, LessThanEqualFloat2) {
  auto source = R"(
    def main() {
      return 3.5 <= 5.0 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, GreaterThanFloat) {
  auto source = R"(
    def main() {
      return 7.5 > 5.0 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, GreaterThanEqualFloat) {
  auto source = R"(
    def main() {
      return 5.0 >= 5.0 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, GreaterThanEqualFloat2) {
  auto source = R"(
    def main() {
      return 7.5 >= 5.0 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, EqualInteger) {
  auto source = R"(
    def main() {
      return 42 == 42 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, NotEqualInteger) {
  auto source = R"(
    def main() {
      return 42 != 43 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, EqualFloat) {
  auto source = R"(
    def main() {
      return 3.5 == 3.5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, NotEqualFloat) {
  auto source = R"(
    def main() {
      return 3.5 != 2.5 ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, EqualBoolean) {
  auto source = R"(
    def main() {
      return true == true ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, NotEqualBoolean) {
  auto source = R"(
    def main() {
      return true != false ? 1 : 0
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, EqualObject) {
  auto source = R"(
    class A {
    }

    def main() {
      var a1 = new A{}
      var a2 = a1
      return equal(a1, a2) ? 1 : 0
    }

    def equal(a, b) {
      return a == b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, NotEqualObject) {
  auto source = R"(
    class A {
    }

    def main() {
      var a1 = new A{}
      var a2 = new A{}
      return not_equal(a1, a2) ? 1 : 0
    }

    def not_equal(a, b) {
      return a != b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, EqualFunction) {
  auto source = R"(
    def main() {
      return equal(foo, foo) ? 1 : 0
    }

    def foo() {
      return 42
    }

    def equal(a, b) {
      return a == b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, NotEqualFunction) {
  auto source = R"(
    def main() {
      return not_equal(foo, bar) ? 1 : 0
    }

    def foo() {
      return 42
    }

    def bar() {
      return 42
    }

    def not_equal(a, b) {
      return a != b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, EqualIncompleteFunction) {
  auto source = R"(
    def main() {
      return equal(foo, foo) ? 1 : 0
    }

    def foo(a) {
      return a
    }

    def equal(a, b) {
      return a == b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, NotEqualIncompleteFunction) {
  auto source = R"(
    def main() {
      return not_equal(foo, bar) ? 1 : 0
    }

    def foo(a) {
      return a
    }

    def bar(a) {
      return a
    }

    def not_equal(a, b) {
      return a != b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, EqualPolymorphicFunction) {
  auto source = R"(
    def main() {
      let a = foo
      let b = foo
      a(10)
      b(10.0)

      return equal(a, b) ? 1 : 0
    }

    def foo(a) {
      return a
    }

    def equal(a, b) {
      return a == b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(OperatorTest, NotEqualPolymorphicFunction) {
  auto source = R"(
    def main() {
      let a = foo
      let b = bar
      a(10)
      b(10.0)

      return not_equal(a, b) ? 1 : 0
    }

    def foo(a) {
      return a
    }

    def bar(a) {
      return a
    }

    def not_equal(a, b) {
      return a != b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, EqualLambda) {
  auto source = R"(
    def main() {
      var a = fn(x) => x + 1
      var b = a

      return equal(a, b) ? 1 : 0
    }

    def equal(a, b) {
      return a == b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, NotEqualLambda) {
  auto source = R"(
    def main() {
      var a = fn(x) => x + 1
      var b = fn(x) => x + 1

      return not_equal(a, b) ? 1 : 0
    }

    def not_equal(a, b) {
      return a != b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, EqualClosure) {
  auto source = R"(
    def main() {
      var factor = 2
      var a = fn(x) => x * factor
      var b = a

      return equal(a, b) ? 1 : 0
    }

    def equal(a, b) {
      return a == b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(OperatorTest, NotEqualClosure) {
  auto source = R"(
    def main() {
      var factor = 2
      var a = fn(x) => x * factor
      var b = fn(x) => x * factor

      return not_equal(a, b) ? 1 : 0
    }

    def not_equal(a, b) {
      return a != b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


}  // namespace xylo
