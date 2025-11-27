
#ifndef XYLO_SYNTAX_IDENTIFIER_H_
#define XYLO_SYNTAX_IDENTIFIER_H_

#include <memory>
#include <utility>

#include "xylo/util/hstring.h"
#include "xylo/util/hstring_view.h"
#include "xylo/util/map.h"


namespace xylo {


class Identifier {
 public:
  explicit Identifier(HString&& str) :
      str_(std::move(str)) {}
  ~Identifier() = default;

  const HString& str() const { return str_; }

 private:
  HString str_;
};


class IdentifierPool {
 private:
  struct IdentStrHash {
    using is_transparent = void;
    size_t operator()(const Identifier* ident) const { return ident->str().hash(); }
    size_t operator()(const HStringView& strview) const { return strview.hash(); }
  };

  struct IdentStrPred {
    using is_transparent = void;
    bool operator()(const Identifier* lhs, const Identifier* rhs) const { return lhs == rhs; }
    bool operator()(const Identifier* lhs, const HStringView& rhs) const { return rhs.equals(lhs->str()); }
    bool operator()(const HStringView& lhs, const Identifier* rhs) const { return lhs.equals(rhs->str()); }
  };

 public:
  IdentifierPool() :
      pool_() {}

  ~IdentifierPool() = default;

  IdentifierPool(const IdentifierPool&) = delete;
  IdentifierPool& operator=(const IdentifierPool&) = delete;

  Identifier* Intern(const HStringView& strview) {
    auto it = pool_.find(strview);
    if (it == pool_.end()) {
      auto ident = std::make_unique<Identifier>(strview.to_str());
      it = pool_.emplace(ident.get(), std::move(ident)).first;
    }
    return it->first;
  }

 private:
  Map<Identifier*, std::unique_ptr<Identifier>, IdentStrHash, IdentStrPred> pool_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_IDENTIFIER_H_
