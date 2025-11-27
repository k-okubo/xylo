
#ifndef XYLO_SYNTAX_FILE_H_
#define XYLO_SYNTAX_FILE_H_

#include <cstddef>

namespace xylo {


class SourceFile {
 public:
  explicit SourceFile(const char* path) :
      path_(path),
      content_(nullptr),
      size_(0) {}

  ~SourceFile() { UnloadContent(); }

  const char* path() const { return path_; }
  const char* content() const { return content_; }
  size_t size() const { return size_; }

  bool LoadContent();
  void UnloadContent();
  bool is_loaded() const { return content_ != nullptr; }

 private:
  const char* path_;
  const char* content_;
  size_t size_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_FILE_H_
