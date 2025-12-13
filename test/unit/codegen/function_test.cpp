
#include <gtest/gtest.h>

#include "./compiler_common.h"

namespace xylo {


TEST(FunctionTest, EmptyFunction) {
  auto source = R"(
    def main() {
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(FunctionTest, ReturnInteger) {
  auto source = R"(
    def main() {
      return 42
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(FunctionTest, RetrunVoid) {
  auto source = R"(
    def main() {
      return
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(FunctionTest, FunctionCall_ImplicitTyvar) {
  auto source = R"(
    def main() {
      return add(40, 2)
    }

    def add(a, b) {
      return a + b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(FunctionTest, FunctionCall_ExplicitType) {
  auto source = R"(
    def main() {
      return add(40, 2)
    }

    def add(a: int, b: int): int {
      return a + b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(FunctionTest, PolymorphicFunction) {
  auto source = R"(
    def main() {
      return id(add)(id(20), id(22))
    }

    def id(x) {
      return x
    }

    def add(a, b) {
      return a + b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(FunctionTest, PolymorphicFunction_ToLet) {
  auto source = R"(
    def main() {
      let f = id
      return f(42)
    }

    def id(x) {
      return x
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(FunctionTest, PolymorphicFunction_ToVar) {
  auto source = R"(
    def main() {
      var f = id
      return f(42)
    }

    def id(x) {
      return x
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(FunctionTest, PolymorphicFunction_Unused) {
  auto source = R"(
    def main() {
      let f = id
      return 0
    }

    def id(x) {
      return x
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(FunctionTest, PolymorphicFunction_Undetermined) {
  auto source = R"(
    def main() {
      id(add)
      return 0
    }

    def id(x) {
      return x
    }

    def add(a, b) {
      return a + b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(FunctionTest, RecursiveFunction) {
  auto source = R"(
    def main() {
      return factorial(5)
    }

    def factorial(n: int): int {
      return n == 0 ? 1 : n * factorial(n - 1)
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 120);
}


TEST(FunctionTest, RecursiveFunction_FunctionAsParameter) {
  auto source = R"(
    def main() {
      return factorial(5, fn(x) => x)
    }

    def factorial(n: int, callback: int -> int): int {
      return n == 0 ? callback(1) : n * factorial(n - 1, callback)
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 120);
}


TEST(FunctionTest, MutualRecursion) {
  auto source = R"(
    def main() {
      return is_even(10) ? 1 : 0
    }

    def is_even(n: int): bool {
      return n == 0 ? true : is_odd(n - 1)
    }

    def is_odd(n: int): bool {
      return n == 0 ? false : is_even(n - 1)
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(FunctionTest, MutualRecursion_FunctionAsParameter) {
  auto source = R"(
    def main() {
      let callback = fn(b) => b ? 1 : 0
      return is_even(10, callback)
    }

    def is_even(n: int, callback: bool -> int): int {
      return n == 0 ? callback(true) : is_odd(n - 1, callback)
    }

    def is_odd(n: int, callback: bool -> int): int {
      return n == 0 ? callback(false) : is_even(n - 1, callback)
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 1);
}


TEST(FunctionTest, Closure_CaptureParameter) {
  auto source = R"(
    def main() {
      return closure_test(42)
    }

    def closure_test(x) {
      return get_x()

      def get_x() {
        return x
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(FunctionTest, Closure_CaptureParameter_ReturnClosure) {
  auto source = R"(
    def main() {
      return closure_test(42)()
    }

    def closure_test(x) {
      return get_x

      def get_x() {
        return x
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


TEST(FunctionTest, Closure_CaptureFunction) {
  auto source = R"(
    def main() {
      return closure_test(10)
    }

    def closure_test(x) {
      return nested_get_x()

      def nested_get_x() {
        return get_x()
      }

      def get_x() {
        return x
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 10);
}


TEST(FunctionTest, Closure_CaptureFunction_ReturnClosure) {
  auto source = R"(
    def main() {
      return closure_test(10)()
    }

    def closure_test(x) {
      return nested_get_x()

      def nested_get_x() {
        return get_x
      }

      def get_x() {
        return x
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 10);
}


TEST(FunctionTest, Closure_DeepCapture) {
  auto source = R"(
    def main() {
      return outer(5)
    }

    def outer(a) {
      return middle()

      def middle() {
        return inner(30)

        def inner(b) {
          return a + b
        }
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 35);
}


TEST(FunctionTest, Closure_LetCapture) {
  auto source = R"(
    def main() {
      return closure_test()
    }

    def closure_test() {
      let x = 50
      let get_x = fn() { return x }
      return get_x()
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 50);
}


TEST(FunctionTest, Closure_VarCapture) {
  auto source = R"(
    def main() {
      return closure_test()
    }

    def closure_test() {
      var x = 100
      let increment_x = fn() { x = x + 1 }
      increment_x()
      increment_x()
      return x
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 102);
}


TEST(FunctionTest, CurriedFunction) {
  auto source = R"(
    def main() {
      return add(19)(23);
    }

    def add(a)(b) {
      return a + b
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 42);
}


}  // namespace xylo
