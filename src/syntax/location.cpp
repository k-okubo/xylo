
#include "xylo/syntax/location.h"

namespace xylo {


void SourceRange::Print(std::ostream& stream) const {
  if (file == nullptr) {
    return;
  }
  bool loaded_before = file->is_loaded();
  if (!loaded_before) {
    if (!file->LoadContent()) {
      return;
    }
  }

  const char* line_start = file->content() + start.offset;
  while (line_start > file->content() && *(line_start - 1) != '\n' && *(line_start - 1) != '\r') {
    line_start--;
  }
  const char* line_end = line_start;
  while (*line_end != '\n' && *line_end != '\r' && *line_end != '\0') {
    line_end++;
  }

  stream.write(line_start, line_end - line_start);
  stream << std::endl;

  for (size_t i = 1; i < start.column; i++) {
    stream << ' ';
  }
  stream << '^';
  if (end.line == start.line && end.column > start.column + 1) {
    for (size_t i = start.column + 1; i < end.column; i++) {
      stream << '~';
    }
  }
  stream << std::endl;

  if (!loaded_before) {
    file->UnloadContent();
  }
}

}  // namespace xylo
