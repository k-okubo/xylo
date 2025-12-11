
#ifndef XYLO_SYNTAX_SCOPE_H_
#define XYLO_SYNTAX_SCOPE_H_

#include <memory>

#include "xylo/util/vector.h"

namespace xylo {


class Scope;
using ScopePtr = std::unique_ptr<Scope>;

class Scope {
 public:
  static ScopePtr CreateRoot() { return std::make_unique<Scope>(); }

  Scope() :
      parent_(nullptr),
      children_(),
      tin_(0),
      tout_(0) {}

  ~Scope() = default;

  Scope(const Scope&) = delete;
  Scope& operator=(const Scope&) = delete;

  Scope* parent() const { return parent_; }

  Scope* CreateChild() {
    auto s = new Scope();
    s->parent_ = this;
    children_.push_back(ScopePtr(s));
    return s;
  }

  bool is_outer_than(const Scope* other) const { return tin_ < other->tin_ && other->tout_ < tout_; }
  bool is_outer_than_equal(const Scope* other) const { return tin_ <= other->tin_ && other->tout_ <= tout_; }
  bool is_inner_than(const Scope* other) const { return other->is_outer_than(this); }
  bool is_inner_than_equal(const Scope* other) const { return other->is_outer_than_equal(this); }

  void Tour() {
    int time = 1;
    Tour(&time);
  }

 protected:
  void Tour(int* time) {
    tin_ = (*time)++;
    for (auto& child : children_) {
      child->Tour(time);
    }
    tout_ = (*time)++;
  }

 private:
  Scope* parent_;
  Vector<ScopePtr> children_;
  int tin_;
  int tout_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_SCOPE_H_
