
#ifndef XYLO_UTIL_FINALLY_H_
#define XYLO_UTIL_FINALLY_H_

#include <utility>

namespace xylo {


template <typename F>
class Finally {
 public:
  explicit Finally(F&& func) :
      func_(std::forward<F>(func)),
      invoked_(false) {}

  ~Finally() { invoke(); }

  Finally(const Finally&) = delete;
  Finally& operator=(const Finally&) = delete;

  Finally(Finally&&) = delete;
  Finally& operator=(Finally&&) = delete;

  void* operator new(size_t) = delete;
  void operator delete(void*) = delete;

  void invoke() {
    if (!invoked_) {
      func_();
      invoked_ = true;
    }
  }

  void cancel() { invoked_ = true; }

 private:
  F func_;
  bool invoked_;
};


}  // namespace xylo

#endif  // XYLO_UTIL_FINALLY_H_
