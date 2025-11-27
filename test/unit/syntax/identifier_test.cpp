
#include "xylo/syntax/identifier.h"

#include <gtest/gtest.h>

namespace xylo {

static_assert(!std::copyable<Identifier>);
static_assert(!std::movable<Identifier>);
static_assert(!std::copyable<IdentifierPool>);
static_assert(!std::movable<IdentifierPool>);


TEST(IdentifierTest, Basic) {
  IdentifierPool pool;
  auto buffer = "foo bar foo";

  auto ident1 = pool.Intern(HStringView(buffer + 0, 3));  // "foo"
  auto ident2 = pool.Intern(HStringView(buffer + 4, 3));  // "bar"
  auto ident3 = pool.Intern(HStringView(buffer + 8, 3));  // "foo"

  EXPECT_EQ(ident1->str().cpp_str(), "foo");
  EXPECT_EQ(ident2->str().cpp_str(), "bar");
  EXPECT_NE(ident1, ident2);
  EXPECT_EQ(ident1, ident3);
}


}  // namespace xylo
