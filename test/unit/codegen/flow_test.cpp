
#include <gtest/gtest.h>

#include "./compiler_common.h"

namespace xylo {


TEST(FlowTest, IfStatementLike) {
  auto source = R"(
    def main() {
      if (true) foo() else bar()
    }

    def foo() {
    }

    def bar() {
      return 20
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(FlowTest, IfStatementLike_ThenOnly) {
  auto source = R"(
    def main() {
      if (true) foo()
    }

    def foo() {
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 0);
}


TEST(FlowTest, ReturnBranch) {
  auto source = R"(
    def main() {
      if (true) {
        return 10
      }

      return 20
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 10);
}


TEST(FlowTest, ReturnBranch_Both) {
  auto source = R"(
    def main() {
      if (false) {
        return 10
      } else {
        return 20
      }
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 20);
}


TEST(FlowTest, NestedBranchedValue) {
  auto source = R"(
    def main() {
      let a = if (true) {
        if (false) {
          10
        } else {
          20
        }
      } else {
        30
      }

      return a
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 20);
}


TEST(FlowTest, HalfReachable) {
  auto source = R"(
    def main() {
      let a = if (true) {
        10
      } else {
        return 20
      }

      return a
    }
  )";

  auto result = CompileAndRun(source);
  EXPECT_EQ(result, 10);
}


}  // namespace xylo
