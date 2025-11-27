
#include "xylo/syntax/file.h"

#include <fstream>

namespace xylo {


bool SourceFile::LoadContent() {
  if (is_loaded()) {
    return true;
  }

  std::ifstream file(path(), std::ios::binary);
  if (!file.is_open()) {
    return false;
  }

  file.seekg(0, std::ios::end);
  std::streamsize size = file.tellg();
  file.seekg(0, std::ios::beg);

  auto buffer = new char[size + 1];

  if (!file.read(buffer, size)) {
    delete[] buffer;
    return false;
  }
  buffer[size] = '\0';

  content_ = buffer;
  size_ = static_cast<size_t>(size);

  return true;
}


void SourceFile::UnloadContent() {
  if (content_) {
    delete[] content_;
    content_ = nullptr;
    size_ = 0;
  }
}


}  // namespace xylo
