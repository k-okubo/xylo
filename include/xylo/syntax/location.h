
#ifndef XYLO_SYNTAX_LOCATION_H_
#define XYLO_SYNTAX_LOCATION_H_

#include <cstddef>
#include <iostream>

#include "xylo/syntax/file.h"

namespace xylo {


struct SourceLocation {
  size_t offset;
  size_t line;
  size_t column;

  SourceLocation(size_t offset, size_t line, size_t column) :
      offset(offset),
      line(line),
      column(column) {}
};


struct SourceRange {
  SourceFile* file;
  SourceLocation start;
  SourceLocation end;

  SourceRange() :
      file(nullptr),
      start(0, 0, 0),
      end(0, 0, 0) {}

  explicit SourceRange(SourceFile* file) :
      file(file),
      start(0, 0, 0),
      end(0, 0, 0) {}

  SourceRange(const SourceRange& start, const SourceRange& end) :
      file(start.file),
      start(start.start),
      end(end.end) {}

  void Print(std::ostream& stream) const;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_LOCATION_H_:w
