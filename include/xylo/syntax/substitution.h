
#ifndef XYLO_SYNTAX_SUBSTITUTION_H_
#define XYLO_SYNTAX_SUBSTITUTION_H_

#include <memory>

#include "xylo/util/map.h"

namespace xylo {


class Type;
class TypeVariable;
class TypeArena;

class Substitution {
 public:
  explicit Substitution(const Substitution* parent = nullptr) :
      parent_(parent) {}
  ~Substitution() = default;

  Substitution(const Substitution&) = delete;
  Substitution& operator=(const Substitution&) = delete;

  auto parent() const { return parent_; }

  void insert(const TypeVariable* var, Type* type) { subst_.emplace(var, type); }
  const Map<const TypeVariable*, Type*>& entries() const { return subst_; }

  Type* Apply(Type* type, TypeArena* arena) const;

 private:
  const Substitution* parent_;
  Map<const TypeVariable*, Type*> subst_;
};

using SubstitutionPtr = std::unique_ptr<Substitution>;


}  // namespace xylo

#endif  // XYLO_SYNTAX_SUBSTITUTION_H_
