
#include <gtest/gtest.h>

#include "xylo/syntax/type.h"

namespace xylo {


TEST(TypePrinterTest, VarName) {
  int n = 21;
  auto types = new Type*[n];

  for (int i = 0; i < n; ++i) {
    types[i] = new TypeVariable(0);
  }
  for (int i = 0; i < n - 1; ++i) {
    types[i]->ConstrainSubtypeOf(types[i + 1]);
  }

  TypePrinter printer;
  TypePrinter::NameMap name_map;
  printer(types[0], &name_map);  // to populate name_map

  EXPECT_EQ(printer(types[0], &name_map), "A");
  EXPECT_EQ(printer(types[1], &name_map), "B");
  EXPECT_EQ(printer(types[17], &name_map), "R");
  EXPECT_EQ(printer(types[18], &name_map), "S");
  EXPECT_EQ(printer(types[19], &name_map), "T1");
  EXPECT_EQ(printer(types[20], &name_map), "T2");

  for (int i = 0; i < n; ++i) {
    delete types[i];
  }
  delete[] types;
}


}  // namespace xylo
