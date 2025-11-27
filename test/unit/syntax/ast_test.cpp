
#include "xylo/syntax/ast.h"

#include <gtest/gtest.h>

namespace xylo {

static_assert(!std::copyable<Symbol>);
static_assert(!std::movable<Symbol>);

static_assert(!std::copyable<FileAST>);
static_assert(!std::movable<FileAST>);

static_assert(!std::copyable<Declaration>);
static_assert(!std::movable<Declaration>);

static_assert(!std::copyable<Block>);
static_assert(!std::movable<Block>);

static_assert(!std::copyable<Expression>);
static_assert(!std::movable<Expression>);

static_assert(!std::copyable<Statement>);
static_assert(!std::movable<Statement>);


}  // namespace xylo
