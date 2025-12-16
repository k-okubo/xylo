
#include "xylo/syntax/type.h"

#include <functional>

#include "xylo/util/finally.h"

namespace xylo {


MemberInfo* NominalType::GetMember(Identifier* member_name, bool include_super) const {
  {
    auto member = GetDeclaredMember(member_name);
    if (member != nullptr) {
      return member;
    }
  }

  {
    Vector<MemberInfo*> embeded_members;
    FindEmbededMembers(member_name, &embeded_members);
    if (embeded_members.size() == 1) {
      return embeded_members[0];
    }
  }

  if (include_super) {
    for (auto super : supers()) {
      auto member = super->GetMember(member_name);
      if (member != nullptr) {
        return member;
      }
    }
  }

  return nullptr;
}


MemberInfo* NominalType::GetDeclaredMember(Identifier* member_name) const {
  auto it = member_map_.find(member_name);
  if (it != member_map_.end()) {
    return it->second.get();
  }
  return nullptr;
}


bool NominalType::GetMemberPath(Identifier* member_name, Vector<NominalSlot*>* out_path) const {
  {
    auto member = GetDeclaredMember(member_name);
    if (member != nullptr) {
      out_path->push_back(member);
      return true;
    }
  }

  for (auto embedded : embeddeds_) {
    auto embedded_type = embedded->type()->As<NominalType>();
    if (embedded_type->GetMemberPath(member_name, out_path)) {
      out_path->push_back(embedded);  // reverse order
      return true;
    }
  }

  for (auto& super : super_infos_) {
    auto super_type = super->type();
    if (super_type->GetMemberPath(member_name, out_path)) {
      out_path->push_back(super.get());  // reverse order
      return true;
    }
  }

  return false;
}


bool NominalType::GetSuperPath(NominalType* super, Vector<NominalSlot*>* out_path) const {
  if (this == super) {
    return true;
  }

  for (auto& direct_super : super_infos_) {
    auto super_type = direct_super->type();
    if (super_type->GetSuperPath(super, out_path)) {
      out_path->push_back(direct_super.get());  // reverse order
      return true;
    }
  }

  return false;
}


void NominalType::FindEmbededMembers(Identifier* member_name, Vector<MemberInfo*>* out_members) const {
  for (auto embedded : embeddeds_) {
    auto embedded_type = embedded->type()->As<NominalType>();
    auto member = embedded_type->GetDeclaredMember(member_name);
    if (member != nullptr) {
      out_members->push_back(member);
    } else {
      embedded_type->FindEmbededMembers(member_name, out_members);
    }
  }
}


void NominalType::FindSuperMembers(Identifier* member_name, Vector<MemberInfo*>* out_members) const {
  for (auto super : supers_) {
    auto member = super->GetDeclaredMember(member_name);
    if (member != nullptr) {
      out_members->push_back(member);
    }
    super->FindSuperMembers(member_name, out_members);
  }
}


void NominalType::FindSuperMethods(Vector<MemberInfo*>* out_members) const {
  for (auto super : supers_) {
    for (auto method : super->methods()) {
      out_members->push_back(method);
    }
    super->FindSuperMethods(out_members);
  }
}


MemberInfo* NominalType::AddField(Identifier* field_name, Type* field_type) {
  if (GetDeclaredMember(field_name) != nullptr) {
    return nullptr;
  }

  // field and embedded share the same numbering system
  int index = fields_.size() + embeddeds_.size();
  auto field = new MemberInfo(this, MemberInfo::Kind::kField, index, field_name, field_type);

  fields_.push_back(field);
  member_map_.emplace(field_name, MemberInfoPtr(field));
  return field;
}


MemberInfo* NominalType::AddMethod(Identifier* method_name, Type* method_type) {
  if (GetDeclaredMember(method_name) != nullptr) {
    return nullptr;
  }

  // method and super share the same numbering system
  int index = methods_.size() + supers_.size();
  auto method = new MemberInfo(this, MemberInfo::Kind::kMethod, index, method_name, method_type);

  methods_.push_back(method);
  member_map_.emplace(method_name, MemberInfoPtr(method));
  return method;
}


MemberInfo* NominalType::AddEmbedded(Identifier* field_name, NominalType* clazz) {
  // Due to member copy by substitution, embeddeds must be added before fields
  if (!fields_.empty()) {
    return nullptr;
  }

  if (GetDeclaredMember(field_name) != nullptr) {
    return nullptr;
  }

  // field and embedded share the same numbering system
  int index = fields_.size() + embeddeds_.size();
  auto embedded = new MemberInfo(this, MemberInfo::Kind::kEmbedding, index, field_name, clazz);

  embeddeds_.push_back(embedded);
  member_map_.emplace(field_name, MemberInfoPtr(embedded));
  return embedded;
}


bool NominalType::AddSuper(NominalType* clazz) {
  // Due to index numbering requirements, supers must be added before methods
  if (!methods_.empty()) {
    return false;
  }

  // method and super share the same numbering system
  int index = methods_.size() + supers_.size();
  auto super_info = new SuperInfo(this, index, clazz);

  supers_.push_back(clazz);
  super_infos_.push_back(SuperInfoPtr(super_info));
  return true;
}


bool ErrorType::equals(const Type* other) const {
  return true;
}


bool NominalType::equals(const Type* other) const {
  return this == other;
}


bool MemberConstraint::equals(const Type* other) const {
  return this == other;
}


bool FunctionType::equals(const Type* other) const {
  if (this == other) {
    return true;
  }
  if (other->kind() != Kind::kFunction) {
    return false;
  }
  auto other_func = other->As<FunctionType>();

  return this->params_type()->equals(other_func->params_type()) &&
         this->return_type()->equals(other_func->return_type()) && this->is_closure() == other_func->is_closure();
}


bool TupleType::equals(const Type* other) const {
  if (this == other) {
    return true;
  }
  if (other->kind() != Kind::kTuple) {
    return false;
  }
  auto other_tuple = other->As<TupleType>();

  if (this->elements().size() != other_tuple->elements().size()) {
    return false;
  }

  for (size_t i = 0; i < elements().size(); ++i) {
    if (!this->elements()[i]->equals(other_tuple->elements()[i])) {
      return false;
    }
  }

  return true;
}


static bool EqualsElementSet(const Vector<Type*>& a, const Vector<Type*>& b) {
  if (a.size() != b.size()) {
    return false;
  }

  Set<size_t> matched_b_indices;

  for (size_t i = 0; i < a.size(); ++i) {
    bool found = false;
    for (size_t j = 0; j < b.size(); ++j) {
      if (matched_b_indices.contains(j)) {
        continue;
      }
      if (a[i]->equals(b[j])) {
        matched_b_indices.emplace(j);
        found = true;
        break;
      }
    }
    if (!found) {
      return false;
    }
  }

  return true;
}

bool IntersectionType::equals(const Type* other) const {
  if (this == other) {
    return true;
  }
  if (other->kind() != Kind::kIntersection) {
    return false;
  }
  auto other_inter = other->As<IntersectionType>();

  return EqualsElementSet(this->elements(), other_inter->elements());
}


bool UnionType::equals(const Type* other) const {
  if (this == other) {
    return true;
  }
  if (other->kind() != Kind::kUnion) {
    return false;
  }
  auto other_uni = other->As<UnionType>();

  return EqualsElementSet(this->elements(), other_uni->elements());
}


bool TypeVariable::equals(const Type* other) const {
  return this == other;
}


bool TypeMetavar::equals(const Type* other) const {
  return this == other;
}


bool TypeScheme::equals(const Type* other) const {
  return this == other;
}


bool TypeApplication::equals(const Type* other) const {
  return this == other;
}


template <typename F1, typename F2, typename F3, typename F4, typename F5, typename F6, typename F7, typename F8>
struct SubtypingProc {
  F1 recurse;
  F2 onNominalToMemcon;
  F3 onTyvarBoth;
  F4 onTyvarSrc;
  F5 onTyvarDst;
  F6 onMetavarBoth;
  F7 onMetavarSrc;
  F8 onMetavarDst;
};


template <typename S, typename D, typename P>
static bool ApplySubtypingRules(S src, D dst, const P& proc) {
  // consider the same instance to be a subtype
  if (src == dst) {
    return true;
  }

  // error type is compatible with any type to avoid cascading errors
  if (src->kind() == Type::Kind::kError || dst->kind() == Type::Kind::kError) {
    return true;
  }

  // nominal to nominal
  if (src->kind() == Type::Kind::kNominal && dst->kind() == Type::Kind::kNominal) {
    auto src_nominal = src->template As<NominalType>();
    for (auto src_super : src_nominal->supers()) {
      if (proc.recurse(src_super, dst, proc)) {
        return true;
      }
    }
    return false;
  }

  // nominal to member constraint
  if (src->kind() == Type::Kind::kNominal && dst->kind() == Type::Kind::kMemberConstraint) {
    auto src_nominal = src->template As<NominalType>();
    auto dst_memcon = dst->template As<MemberConstraint>();
    return proc.onNominalToMemcon(src_nominal, dst_memcon);
  }

  // function to function: contravariant in parameter, covariant in return
  if (src->kind() == Type::Kind::kFunction && dst->kind() == Type::Kind::kFunction) {
    auto src_func = src->template As<FunctionType>();
    auto dst_func = dst->template As<FunctionType>();
    if (src_func->is_closure() && !dst_func->is_closure()) {
      return false;
    }
    return proc.recurse(dst_func->params_type(), src_func->params_type(), proc) &&
           proc.recurse(src_func->return_type(), dst_func->return_type(), proc);
  }

  // tuple to tuple: element-wise subtyping
  if (src->kind() == Type::Kind::kTuple && dst->kind() == Type::Kind::kTuple) {
    auto src_tuple = src->template As<TupleType>();
    auto dst_tuple = dst->template As<TupleType>();

    if (src_tuple->elements().size() != dst_tuple->elements().size()) {
      return false;
    }

    auto src_elem_it = src_tuple->elements().begin();
    auto dst_elem_it = dst_tuple->elements().begin();

    while (src_elem_it != src_tuple->elements().end() && dst_elem_it != dst_tuple->elements().end()) {
      if (!proc.recurse(*src_elem_it, *dst_elem_it, proc)) {
        return false;
      }
      ++src_elem_it;
      ++dst_elem_it;
    }

    return true;
  }

  // tuple is not compatible with other types including tyvar/metavar
  if (src->kind() == Type::Kind::kTuple || dst->kind() == Type::Kind::kTuple) {
    return false;
  }

  // type scheme is not compatible with any type
  if (src->kind() == Type::Kind::kScheme || dst->kind() == Type::Kind::kScheme) {
    return false;
  }

  // anything to intersection: Forall elements in dst, src <: the element
  // behaves like TopType if elements is empty
  if (dst->kind() == Type::Kind::kIntersection) {
    auto dst_inter = dst->template As<IntersectionType>();
    for (size_t i = 0; i < dst_inter->elements().size(); ++i) {
      auto dst_elem = dst_inter->elements()[i];
      if (!proc.recurse(src, dst_elem, proc)) {
        return false;
      }
    }
    return true;
  }

  // intersection to anything: Exists element in src, the element <: dst
  if (src->kind() == Type::Kind::kIntersection) {
    auto src_inter = src->template As<IntersectionType>();
    for (size_t i = 0; i < src_inter->elements().size(); ++i) {
      auto src_elem = src_inter->elements()[i];
      if (proc.recurse(src_elem, dst, proc)) {
        return true;
      }
    }
    return false;
  }

  // union to anything: Forall elements in src, the element <: dst
  // behaves like BottomType if elements is empty
  if (src->kind() == Type::Kind::kUnion) {
    auto src_union = src->template As<UnionType>();
    for (size_t i = 0; i < src_union->elements().size(); ++i) {
      auto src_elem = src_union->elements()[i];
      if (!proc.recurse(src_elem, dst, proc)) {
        return false;
      }
    }
    return true;
  }

  // anything to union: Exists element in dst, src <: the element
  if (dst->kind() == Type::Kind::kUnion) {
    auto dst_union = dst->template As<UnionType>();
    for (size_t i = 0; i < dst_union->elements().size(); ++i) {
      auto dst_elem = dst_union->elements()[i];
      if (proc.recurse(src, dst_elem, proc)) {
        return true;
      }
    }
    return false;
  }

  // metavar accepts any other types
  {
    if (src->kind() == Type::Kind::kMetavar && dst->kind() == Type::Kind::kMetavar) {
      auto src_metavar = src->template As<TypeMetavar>();
      auto dst_metavar = dst->template As<TypeMetavar>();
      return proc.onMetavarBoth(src_metavar, dst_metavar);
    }

    if (src->kind() == Type::Kind::kMetavar) {
      auto src_metavar = src->template As<TypeMetavar>();
      return proc.onMetavarSrc(src_metavar, dst);
    }

    if (dst->kind() == Type::Kind::kMetavar) {
      auto dst_metavar = dst->template As<TypeMetavar>();
      return proc.onMetavarDst(src, dst_metavar);
    }
  }

  // tyvar accepts nominal, function, and tyvar
  {
    if (src->kind() == Type::Kind::kTyvar && dst->kind() == Type::Kind::kTyvar) {
      auto src_var = src->template As<TypeVariable>();
      auto dst_var = dst->template As<TypeVariable>();
      return proc.onTyvarBoth(src_var, dst_var);
    }

    if (src->kind() == Type::Kind::kTyvar) {
      auto src_var = src->template As<TypeVariable>();
      return proc.onTyvarSrc(src_var, dst);
    }

    if (dst->kind() == Type::Kind::kTyvar) {
      auto dst_var = dst->template As<TypeVariable>();
      return proc.onTyvarDst(src, dst_var);
    }
  }

  return false;
}


bool Type::IsSubtypeOf(const Type* dst, TypePairSet* visited) const {
  auto src = this;

  // if upper/lower bound of tyvar/metavar is circularly, treat it as having no bound
  if (visited->contains({src, dst})) {
    return false;
  }
  visited->emplace(std::pair(src, dst));
  Finally _([=] {
    visited->erase({src, dst});
  });

  auto onVarSrc = [=](auto s, auto d) {
    auto func_shape = s->func_shape();
    if (func_shape != nullptr) {
      return func_shape->IsSubtypeOf(d, visited);
    } else {
      return s->upper_bound()->IsSubtypeOf(d, visited);
    }
  };

  auto onVarDst = [=](auto s, auto d) {
    auto func_shape = d->func_shape();
    if (func_shape != nullptr) {
      return s->IsSubtypeOf(func_shape, visited);
    } else {
      return s->IsSubtypeOf(d->lower_bound(), visited);
    }
  };

  auto proc = SubtypingProc{
    .recurse =
        [=](const Type* s, const Type* d, const auto& p) {
          return s->IsSubtypeOf(d, visited);
        },

    .onNominalToMemcon =
        [=](const NominalType* nominal, const MemberConstraint* memcon) {
          return false;
        },

    .onTyvarBoth = onVarSrc,
    .onTyvarSrc = onVarSrc,
    .onTyvarDst = onVarDst,
    .onMetavarBoth = onVarSrc,
    .onMetavarSrc = onVarSrc,
    .onMetavarDst = onVarDst,
  };

  return ApplySubtypingRules(src, dst, proc);
}


bool Type::CanConstrainSubtypeOf(const Type* dst, TypePairSet* visited) const {
  auto src = this;

  // if upper/lower bound of tyvar/metavar is circularly, treat it as having no bound
  if (visited->contains({src, dst})) {
    return true;
  }
  visited->emplace(std::pair(src, dst));
  Finally _([=] {
    visited->erase({src, dst});
  });

  auto onVarSrc = [=](auto s, auto d) {
    return s->CanConstrainUpperBound(d, visited);
  };

  auto onVarDst = [=](auto s, auto d) {
    return d->CanConstrainLowerBound(s, visited);
  };

  auto proc = SubtypingProc{
    .recurse =
        [=](const Type* s, const Type* d, const auto& p) {
          return s->CanConstrainSubtypeOf(d, visited);
        },

    .onNominalToMemcon =
        [=](const NominalType* nominal, const MemberConstraint* memcon) {
          return memcon->CanResolve(nominal, visited);
        },

    .onTyvarBoth = onVarSrc,
    .onTyvarSrc = onVarSrc,
    .onTyvarDst = onVarDst,
    .onMetavarBoth = onVarSrc,
    .onMetavarSrc = onVarSrc,
    .onMetavarDst = onVarDst,
  };

  return ApplySubtypingRules(src, dst, proc);
}


bool Type::ConstrainSubtypeOf(Type* dst, TypePairSet* visited) {
  auto src = this;

  if (visited->contains({src, dst})) {
    return true;
  }
  visited->emplace(std::pair(src, dst));

  // pre-check
  if (src->is_var_type() || dst->is_var_type()) {
    if (!CanConstrainSubtypeOf(dst)) {
      return false;
    }
  }

  auto onVarSrc = [=](auto s, auto d) {
    return s->ConstrainUpperBound(d, visited);
  };

  auto onVarDst = [=](auto s, auto d) {
    return d->ConstrainLowerBound(s, visited);
  };

  auto onVarBoth = [=](auto s, auto d) {
    return s->ConstrainBothBounds(d, visited);
  };

  auto proc = SubtypingProc{
    .recurse =
        [=](Type* s, Type* d, const auto& p) {
          return s->ConstrainSubtypeOf(d, visited);
        },

    .onNominalToMemcon =
        [=](NominalType* nominal, MemberConstraint* memcon) {
          return memcon->Resolve(nominal, visited);
        },

    .onTyvarBoth = onVarBoth,
    .onTyvarSrc = onVarSrc,
    .onTyvarDst = onVarDst,
    .onMetavarBoth = onVarBoth,
    .onMetavarSrc = onVarSrc,
    .onMetavarDst = onVarDst,
  };

  return ApplySubtypingRules(src, dst, proc);
}


bool Type::ConstrainSameAs(Type* other) {
  return this->ConstrainSubtypeOf(other) && other->ConstrainSubtypeOf(this);
}


template <typename F>
static void ForeachSupertype(Type* t, F proc) {
  if (t->kind() == Type::Kind::kNominal) {
    auto nominal = t->As<NominalType>();
    for (auto super : nominal->supers()) {
      proc(super);
    }
    return;
  }

  throw std::logic_error("not implemented: VisitSupertype for such a type");
}


static void PutCommonAncestorsOfPair(Type* t1, Type* t2, Vector<Type*>* ancestors) {
  if (t2->IsSubtypeOf(t1)) {
    ancestors->push_back(t1);
    return;
  }
  if (t1->IsSubtypeOf(t2)) {
    ancestors->push_back(t2);
    return;
  }

  // traverse supertypes to find a common ancestor
  ForeachSupertype(t1, [=](Type* t1_super) {
    PutCommonAncestorsOfPair(t1_super, t2, ancestors);
  });
}


static void FindCommonAncestors(const UnionType* types, Vector<Type*>* out_ancestors) {
  if (types->empty()) {
    return;
  }

  Vector<Type*> partial_ancestors;
  UnionType bottom_type;  // empty union behaves bottom type
  partial_ancestors.push_back(&bottom_type);

  for (auto elem : types->elements()) {
    Vector<Type*> partial_ancestors_next;
    for (auto a : partial_ancestors) {
      PutCommonAncestorsOfPair(a, elem, &partial_ancestors_next);
    }
    partial_ancestors = std::move(partial_ancestors_next);
  }

  *out_ancestors = std::move(partial_ancestors);
}


bool MemberConstraint::CanResolve(const NominalType* nominal, TypePairSet* visited) const {
  auto [owner, member_info] = FindConstraintMember(const_cast<NominalType*>(nominal));
  if (owner == nullptr) {
    return false;
  }

  TypeArena arena;
  Type* member_type = member_info->type()->Instantiate(&arena);

  if (!member_type->CanConstrainSubtypeOf(expected_type(), visited)) {
    CallbackOnError(owner);
    return false;
  }
  if (!expected_type()->CanConstrainSubtypeOf(member_type, visited)) {
    CallbackOnError(owner);
    return false;
  }

  return true;
}


bool MemberConstraint::Resolve(NominalType* nominal, TypePairSet* visited) {
  auto [owner, member_info] = FindConstraintMember(nominal);
  if (owner == nullptr) {
    return false;
  }

  TypeArena arena;
  Type* member_type = member_info->type()->Instantiate(&arena);

  if (!member_type->ConstrainSameAs(expected_type())) {
    CallbackOnError(owner);
    return false;
  }

  set_resolved_in(owner);
  set_resolved_member(member_info);
  this->arena()->merge(&arena);

  return true;
}


std::pair<NominalType*, MemberInfo*> MemberConstraint::FindConstraintMember(NominalType* nominal) const {
  NominalType* owner = nullptr;
  MemberInfo* member = nullptr;

  if (!is_resolved()) {
    owner = nominal;
    member = nominal->GetMember(member_name());
    if (member == nullptr) {
      CallbackOnError(owner);
      return {nullptr, nullptr};
    }

  } else {
    Vector<Type*> ancestors;
    PutCommonAncestorsOfPair(resolved_in_, nominal, &ancestors);

    if (ancestors.empty()) {
      // no common ancestor; no need to report error here
      return {nullptr, nullptr};
    }

    for (auto anc : ancestors) {
      auto anc_nominal = anc->As<NominalType>();
      auto anc_member = anc_nominal->GetMember(member_name());
      if (anc_member == nullptr) {
        continue;
      }
      if (owner != nullptr) {
        // ambiguous member
        CallbackOnError(owner, anc_nominal);
        return {nullptr, nullptr};
      }
      owner = anc_nominal;
      member = anc_member;
    }

    if (owner == nullptr) {
      CallbackOnError(nominal);
      return {nullptr, nullptr};
    }
  }

  if (member->type() == nullptr) {
    member->resolve_type();
    xylo_contract(member->type() != nullptr);
  }

  if (is_mutable() && member->kind() != MemberInfo::Kind::kField) {
    CallbackOnError(owner);
    return {nullptr, nullptr};
  }

  return {owner, member};
}


bool MemberConstraint::SetMutable(bool m) {
  if (m && is_resolved() && resolved_member()->kind() != MemberInfo::Kind::kField) {
    return false;
  }

  mutable_ = m;
  return true;
}


static bool OccursIn(const Type* haystack, const Type* needle, bool check_ul_bound) {
  switch (haystack->kind()) {
    case Type::Kind::kError:
      return false;

    case Type::Kind::kNominal:
      return false;

    case Type::Kind::kMemberConstraint:
      return false;

    case Type::Kind::kFunction: {
      auto func = haystack->As<FunctionType>();
      if (OccursIn(func->params_type(), needle, true)) {
        return true;
      }
      if (OccursIn(func->return_type(), needle, true)) {
        return true;
      }
      return false;
    }

    case Type::Kind::kTuple: {
      auto tuple = haystack->As<TupleType>();
      for (auto elem : tuple->elements()) {
        if (OccursIn(elem, needle, true)) {
          return true;
        }
      }
      return false;
    }

    case Type::Kind::kIntersection: {
      auto inter = haystack->As<IntersectionType>();
      for (auto elem : inter->elements()) {
        if (OccursIn(elem, needle, check_ul_bound)) {
          return true;
        }
      }
      return false;
    }

    case Type::Kind::kUnion: {
      auto uni = haystack->As<UnionType>();
      for (auto elem : uni->elements()) {
        if (OccursIn(elem, needle, check_ul_bound)) {
          return true;
        }
      }
      return false;
    }

    case Type::Kind::kTyvar: {
      if (haystack == needle) {
        return true;
      }

      auto tyvar = haystack->As<TypeVariable>();
      if (check_ul_bound) {
        if (OccursIn(tyvar->upper_bound(), needle, false)) {
          return true;
        }
        if (OccursIn(tyvar->lower_bound(), needle, false)) {
          return true;
        }
      }
      return false;
    }

    case Type::Kind::kMetavar: {
      if (haystack == needle) {
        return true;
      }

      auto metavar = haystack->As<TypeMetavar>();
      if (check_ul_bound) {
        if (OccursIn(metavar->upper_bound(), needle, false)) {
          return true;
        }
        if (OccursIn(metavar->lower_bound(), needle, false)) {
          return true;
        }
      }
      return false;
    }

    case Type::Kind::kScheme:
      xylo_unreachable();

    case Type::Kind::kApplication:
      xylo_unreachable();
  }

  xylo_unreachable();
}


static void CollectTopAncestors(Type* type, UnionType* ancestors) {
  switch (type->kind()) {
    case Type::Kind::kError:
      return;

    case Type::Kind::kNominal: {
      auto nominal = type->As<NominalType>();

      if (nominal->supers().size() == 0) {
        ancestors->add_element(nominal);
        return;
      } else {
        for (auto super : nominal->supers()) {
          CollectTopAncestors(super, ancestors);
        }
      }

      return;
    }

    case Type::Kind::kIntersection: {
      auto inter = type->As<IntersectionType>();
      for (auto elem : inter->elements()) {
        CollectTopAncestors(elem, ancestors);
      }
      return;
    }

    case Type::Kind::kTyvar:
    case Type::Kind::kMetavar:
      // no ancestors
      return;

    case Type::Kind::kMemberConstraint:
    case Type::Kind::kFunction:
    case Type::Kind::kTuple:
    case Type::Kind::kUnion:
    case Type::Kind::kScheme:
    case Type::Kind::kApplication:
      xylo_unreachable();
  }
}


bool VariableBase::CanConstrainUpperBound(const Type* new_ub, TypePairSet* visited) const {
  xylo_contract(new_ub->kind() != Type::Kind::kTuple);
  xylo_contract(new_ub->kind() != Type::Kind::kIntersection);
  xylo_contract(new_ub->kind() != Type::Kind::kScheme);
  xylo_contract(!(this->kind() == Type::Kind::kTyvar && new_ub->kind() == Type::Kind::kMetavar));

  if (upper_bound()->IsSubtypeOf(new_ub)) {
    return true;
  }

  // circular types are prohibited
  if (OccursIn(new_ub, this, false)) {
    return false;
  }

  if (!lower_bound()->CanConstrainSubtypeOf(new_ub, visited)) {
    return false;
  }

  if (lower_func_shape() != nullptr) {
    if (!lower_func_shape()->CanConstrainSubtypeOf(new_ub, visited)) {
      return false;
    }
  }

  return true;
}


bool VariableBase::CanConstrainLowerBound(const Type* new_lb, TypePairSet* visited) const {
  xylo_contract(new_lb->kind() != Type::Kind::kMemberConstraint);
  xylo_contract(new_lb->kind() != Type::Kind::kTuple);
  xylo_contract(new_lb->kind() != Type::Kind::kUnion);
  xylo_contract(new_lb->kind() != Type::Kind::kScheme);
  xylo_contract(!(this->kind() == Type::Kind::kTyvar && new_lb->kind() == Type::Kind::kMetavar));
  if (new_lb->IsSubtypeOf(lower_bound())) {
    return true;
  }

  // circular types are prohibited
  if (OccursIn(new_lb, this, false)) {
    return false;
  }

  if (!new_lb->CanConstrainSubtypeOf(upper_bound(), visited)) {
    return false;
  }

  if (upper_func_shape() != nullptr) {
    if (!new_lb->CanConstrainSubtypeOf(upper_func_shape(), visited)) {
      return false;
    }
  }

  return true;
}


bool VariableBase::ConstrainUpperBound(Type* new_ub, TypePairSet* visited) {
  xylo_contract(new_ub->kind() != Type::Kind::kTuple);
  xylo_contract(new_ub->kind() != Type::Kind::kIntersection);
  xylo_contract(new_ub->kind() != Type::Kind::kScheme);
  xylo_contract(!(this->kind() == Type::Kind::kTyvar && new_ub->kind() == Type::Kind::kMetavar));

  if (new_ub->kind() == Type::Kind::kFunction) {
    // already constrained
    if (upper_func_shape() != nullptr && upper_func_shape()->IsSubtypeOf(new_ub)) {
      return true;
    }

    // functionize if needed
    auto new_ub_func = new_ub->As<FunctionType>();
    Functionize(new_ub_func);
    GenUpperBoundFuncShape(new_ub_func, visited);

    // constrain func shape
    if (!upper_func_shape()->ConstrainSubtypeOf(new_ub, visited)) {
      return false;
    }

    // connect func shape to lower side
    if (!lower_bound()->ConstrainSubtypeOf(upper_func_shape(), visited)) {
      return false;
    }

  } else {
    // already constrained
    if (upper_bound()->IsSubtypeOf(new_ub)) {
      return true;
    }

    // new_ub must be a supertype of lower bound
    if (!lower_bound()->ConstrainSubtypeOf(new_ub, visited)) {
      return false;
    }

    // new_ub must be a supertype of lower func shape if functionized
    if (lower_func_shape() != nullptr) {
      if (!lower_func_shape()->ConstrainSubtypeOf(new_ub, visited)) {
        return false;
      }
    }

    AddToUpperBound(new_ub, visited);
  }

  return true;
}


bool VariableBase::ConstrainLowerBound(Type* new_lb, TypePairSet* visited) {
  xylo_contract(new_lb->kind() != Type::Kind::kMemberConstraint);
  xylo_contract(new_lb->kind() != Type::Kind::kTuple);
  xylo_contract(new_lb->kind() != Type::Kind::kUnion);
  xylo_contract(new_lb->kind() != Type::Kind::kScheme);
  xylo_contract(!(this->kind() == Type::Kind::kTyvar && new_lb->kind() == Type::Kind::kMetavar));

  if (new_lb->instantiated_info() != nullptr) {
    xylo_contract(instantiated_info() == nullptr);
    set_instantiated_info(new_lb->instantiated_info());
  }

  if (new_lb->kind() == Type::Kind::kFunction) {
    // already constrained
    if (lower_func_shape() != nullptr && new_lb->IsSubtypeOf(lower_func_shape())) {
      return true;
    }

    // functionize if needed
    auto new_lb_func = new_lb->As<FunctionType>();
    Functionize(new_lb_func);
    GenLowerBoundFuncShape(new_lb_func, visited);

    // constrain func shape
    if (!new_lb->ConstrainSubtypeOf(lower_func_shape(), visited)) {
      return false;
    }

    // connect func shape to upper side
    if (!lower_func_shape()->ConstrainSubtypeOf(upper_bound(), visited)) {
      return false;
    }

  } else {
    // already constrained
    if (new_lb->IsSubtypeOf(lower_bound())) {
      return true;
    }

    // new_lb must be a subtype of upper bound
    if (!new_lb->ConstrainSubtypeOf(upper_bound(), visited)) {
      return false;
    }

    // new_lb must be a subtype of upper func shape if functionized
    if (upper_func_shape() != nullptr) {
      if (!new_lb->ConstrainSubtypeOf(upper_func_shape(), visited)) {
        return false;
      }
    }

    AddToLowerBound(new_lb, visited);
  }

  return true;
}


bool VariableBase::ConstrainBothBounds(VariableBase* dst, TypePairSet* visited) {
  auto src = this;

  // Merged ConstrainLowerBound and ConstrainUpperBound

  if (src->IsSubtypeOf(dst)) {
    return true;
  }

  // xylo_check(src->lower_bound_->ConstrainSubtypeOf(dst->owner_, visited));
  for (auto src_lb : src->lower_bound()->elements()) {
    if (src_lb->kind() != dst->kind()) {
      if (!src_lb->ConstrainSubtypeOf(dst, visited)) {
        return false;
      }
    }
  }

  // xylo_check(src->owner_->ConstrainSubtypeOf(dst->upper_bound_.get(), visited));
  for (auto dst_ub : dst->upper_bound()->elements()) {
    if (dst_ub->kind() != src->kind()) {
      if (!src->ConstrainSubtypeOf(dst_ub, visited)) {
        return false;
      }
    }
  }

  // constrain func_shape if functionized
  if (src->lower_func_shape() != nullptr) {
    if (!src->lower_func_shape()->ConstrainSubtypeOf(dst, visited)) {
      return false;
    }
  }
  if (dst->upper_func_shape() != nullptr) {
    if (!src->ConstrainSubtypeOf(dst->upper_func_shape(), visited)) {
      return false;
    }
  }

  if (!src->AddToUpperBound(dst, visited)) {
    return false;
  }
  if (!dst->AddToLowerBound(src, visited)) {
    return false;
  }

  return true;
}


bool VariableBase::AddToUpperBound(Type* new_ub, TypePairSet* visited) {
  // does not have references to TypeVariables from inner scopes
  if (this->kind() == Type::Kind::kTyvar && new_ub->kind() == Type::Kind::kTyvar) {
    auto this_tyvar = this->As<TypeVariable>();
    auto ub_tyvar = new_ub->As<TypeVariable>();

    if (ub_tyvar->scope()->is_inner_than(this_tyvar->scope())) {
      return true;
    }
  }

  // change type variables in MemberConstraint to outer type variables
  if (this->kind() == Type::Kind::kTyvar && new_ub->kind() == Type::Kind::kMemberConstraint) {
    auto this_tyvar = this->As<TypeVariable>();
    auto ub_memcon = new_ub->As<MemberConstraint>();

    if (ub_memcon->expected_type()->kind() == Type::Kind::kTyvar) {
      auto ub_member_tyvar = ub_memcon->expected_type()->As<TypeVariable>();
      if (this_tyvar->scope()->is_outer_than(ub_member_tyvar->scope())) {
        ub_member_tyvar->set_scope(this_tyvar->scope());
      }
    }
    if (ub_memcon->expected_type()->kind() == Type::Kind::kMetavar) {
      auto ub_member_tyvar = new TypeVariable(this_tyvar->scope());
      ub_member_tyvar->ConstrainSameAs(ub_memcon->expected_type());
      ub_memcon->set_expected_type(ub_member_tyvar);
      this->arena()->adopt_type(TypePtr(ub_member_tyvar));
    }
  }

  upper_bound_->add_element(new_ub);
  return true;
}


bool VariableBase::AddToLowerBound(Type* new_lb, TypePairSet* visited) {
  // does not have references to TypeVariables from inner scopes
  if (this->kind() == Type::Kind::kTyvar && new_lb->kind() == Type::Kind::kTyvar) {
    auto this_tyvar = this->As<TypeVariable>();
    auto lb_tyvar = new_lb->As<TypeVariable>();

    if (lb_tyvar->scope()->is_inner_than(this_tyvar->scope())) {
      return true;
    }
  }

  // Add new_lb's ancestors to the upper bound to enable sibling checking for types added to lb in the future.
  auto ancestors = std::make_unique<UnionType>();
  CollectTopAncestors(new_lb, ancestors.get());

  switch (ancestors->elements().size()) {
    case 0:
      // no ancestors for tyvar/metavar, do nothing.
      break;

    case 1:
      // new_lb has a single ancestor, add it directly.
      if (!ConstrainUpperBound(ancestors->elements()[0], visited)) {
        return false;
      }
      break;

    default:
      // new_lb has multiple ancestors, add the union of them.
      // future new_lb must be descendant of any of them.
      if (!ConstrainUpperBound(ancestors.get(), visited)) {
        return false;
      }
      this->arena()->adopt_type(std::move(ancestors));
      break;
  }

  lower_bound_->add_element(new_lb);
  return true;
}


void VariableBase::Functionize(const FunctionType* base_shape) {
  if (func_shape() != nullptr) {
    return;
  }

  auto params_size = base_shape->params_type()->elements().size();
  auto params = new TupleType();
  this->arena()->adopt_type(TypePtr(params));

  for (size_t i = 0; i < params_size; ++i) {
    auto var = create_var_();  // TODO: use TypeMetavar
    this->arena()->adopt_type(TypePtr(var));
    params->add_element(var);
  }

  auto return_type = create_var_();
  func_shape_ = new FunctionType(false, params, return_type);
  this->arena()->adopt_type(TypePtr(return_type));
  this->arena()->adopt_type(TypePtr(func_shape_));
}


bool VariableBase::GenUpperBoundFuncShape(FunctionType* new_ub, TypePairSet* visited) {
  if (upper_func_shape()) {
    if (!new_ub->is_closure()) {
      upper_func_shape_->set_closure(false);
    }
  } else {
    upper_func_shape_ =
        std::make_unique<FunctionType>(new_ub->is_closure(), func_shape_->params_type(), func_shape_->return_type());
  }

  return true;
}


bool VariableBase::GenLowerBoundFuncShape(FunctionType* new_lb, TypePairSet* visited) {
  if (lower_func_shape()) {
    if (new_lb->is_closure()) {
      lower_func_shape_->set_closure(true);
    }
  } else {
    lower_func_shape_ =
        std::make_unique<FunctionType>(new_lb->is_closure(), func_shape_->params_type(), func_shape_->return_type());
  }

  return true;
}


void IntersectionType::ShrinkToLower() {
  if (shrinked_) {
    return;
  }
  if (elements_.size() <= 1) {
    return;
  }

  Vector<Type*> old_elements = std::move(elements_);
  Vector<Type*> new_elements;

  // copy to new_elements while removing redundant upper elements
  for (size_t i = 0; i < old_elements.size(); ++i) {
    bool redundant = false;

    for (auto new_elem : new_elements) {
      if (new_elem->IsSubtypeOf(old_elements[i])) {
        redundant = true;
        break;
      }
    }
    if (redundant) {
      continue;
    }

    for (size_t j = i + 1; j < old_elements.size(); ++j) {
      if (old_elements[j]->IsSubtypeOf(old_elements[i])) {
        redundant = true;
        break;
      }
    }
    if (redundant) {
      continue;
    }

    new_elements.push_back(old_elements[i]);
  }

  // update elements_
  elements_ = std::move(new_elements);
  shrinked_ = true;
}


void UnionType::ShrinkToUpper() {
  if (shrinked_) {
    return;
  }
  if (elements_.size() <= 1) {
    return;
  }

  Vector<Type*> old_elements = std::move(elements_);
  Vector<Type*> new_elements;

  // copy to new_elements while removing redundant lower elements
  for (size_t i = 0; i < old_elements.size(); ++i) {
    bool redundant = false;

    for (auto new_elem : new_elements) {
      if (old_elements[i]->IsSubtypeOf(new_elem)) {
        redundant = true;
        break;
      }
    }
    if (redundant) {
      continue;
    }

    for (size_t j = i + 1; j < old_elements.size(); ++j) {
      if (old_elements[i]->IsSubtypeOf(old_elements[j])) {
        redundant = true;
        break;
      }
    }
    if (redundant) {
      continue;
    }

    new_elements.push_back(old_elements[i]);
  }

  // update elements_
  elements_ = std::move(new_elements);
  shrinked_ = true;
}


Type* ErrorType::CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) {
  return this;
}


Type* NominalType::CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) {
  if (this->substitution() != nullptr) {
    this->substitution()->CloseOverMetavars(scope, arena, map);

    for (auto field : this->fields()) {
      if (field->type() != nullptr) {
        field->set_type(field->type()->CloseOverMetavars(scope, arena, map));
      }
    }

    for (auto method : this->methods()) {
      if (method->type() != nullptr) {
        method->set_type(method->type()->CloseOverMetavars(scope, arena, map));
      }
    }
  }

  return this;
}


Type* MemberConstraint::CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) {
  return this;
}


Type* FunctionType::CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) {
  params_type()->CloseOverMetavars(scope, arena, map);
  return_type_ = return_type()->CloseOverMetavars(scope, arena, map);
  return this;
}


Type* TupleType::CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) {
  Vector<Type*> new_elements;
  for (auto elem : elements()) {
    auto new_elem = elem->CloseOverMetavars(scope, arena, map);
    new_elements.push_back(new_elem);
  }

  elements_ = std::move(new_elements);
  return this;
}


Type* IntersectionType::CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) {
  Vector<Type*> new_elements;
  for (auto elem : elements()) {
    auto new_elem = elem->CloseOverMetavars(scope, arena, map);
    new_elements.push_back(new_elem);
  }

  elements_ = std::move(new_elements);
  return this;
}


Type* UnionType::CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) {
  Vector<Type*> new_elements;
  for (auto elem : elements()) {
    auto new_elem = elem->CloseOverMetavars(scope, arena, map);
    new_elements.push_back(new_elem);
  }

  elements_ = std::move(new_elements);
  return this;
}


Type* TypeVariable::CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) {
  return this;
}


Type* TypeMetavar::CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) {
  auto it = map->find(this);
  if (it != map->end()) {
    return it->second;
  }

  Substitution subst;
  auto outgoing = this->Zonk(&subst, true, arena);
  if (outgoing->kind() != Type::Kind::kError) {
    outgoing = outgoing->CloseOverMetavars(scope, arena, map);
  } else {
    // The metavar cannot be determined uniquely, so it should be made into a variable.
    outgoing = new TypeVariable(scope);
    this->ConstrainSameAs(outgoing);
    arena->adopt_type(TypePtr(outgoing));
  }

  map->emplace(this, outgoing);
  return outgoing;
}


Type* TypeScheme::CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) {
  return this;
}


Type* TypeApplication::CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) {
  return this;
}


void Substitution::CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) {
  for (auto& [param, arg] : entries_) {
    arg = arg->CloseOverMetavars(scope, arena, map);
  }
}


struct TyvarCollection {
  Set<TypeVariable*> set;
  Vector<TypeVariable*> vec;

  void insert(TypeVariable* tyvar) {
    auto [_, first_time] = set.emplace(tyvar);
    if (first_time) {
      vec.push_back(tyvar);
    }
  }
};


static void CollectFreeTyvars(Scope* scope, Substitution* subst, TyvarCollection* out_ftv);

static void CollectFreeTyvars(Scope* scope, Type* type, TyvarCollection* out_ftv) {
  switch (type->kind()) {
    case Type::Kind::kError:
      return;

    case Type::Kind::kNominal: {
      auto nominal = type->As<NominalType>();
      if (nominal->substitution() != nullptr) {
        CollectFreeTyvars(scope, nominal->substitution(), out_ftv);
      }
      return;
    }

    case Type::Kind::kMemberConstraint: {
      auto memcon = type->As<MemberConstraint>();
      CollectFreeTyvars(scope, memcon->expected_type(), out_ftv);
      return;
    }

    case Type::Kind::kFunction: {
      auto func = type->As<FunctionType>();
      CollectFreeTyvars(scope, func->params_type(), out_ftv);
      CollectFreeTyvars(scope, func->return_type(), out_ftv);
      return;
    }

    case Type::Kind::kTuple: {
      auto tuple = type->As<TupleType>();
      for (auto elem : tuple->elements()) {
        CollectFreeTyvars(scope, elem, out_ftv);
      }
      return;
    }

    case Type::Kind::kIntersection: {
      auto inter = type->As<IntersectionType>();
      for (auto elem : inter->elements()) {
        CollectFreeTyvars(scope, elem, out_ftv);
      }
      return;
    }

    case Type::Kind::kUnion: {
      auto uni = type->As<UnionType>();
      for (auto elem : uni->elements()) {
        CollectFreeTyvars(scope, elem, out_ftv);
      }
      return;
    }

    case Type::Kind::kTyvar: {
      auto tyvar = type->As<TypeVariable>();
      if (tyvar->scope() == scope) {
        out_ftv->insert(tyvar);
      } else {
        xylo_contract(tyvar->scope()->is_outer_than(scope));
      }

      return;
    }

    case Type::Kind::kMetavar:
    case Type::Kind::kScheme:
    case Type::Kind::kApplication:
      xylo_unreachable();
  }
}


static void CollectFreeTyvars(Scope* scope, Substitution* subst, TyvarCollection* out_ftv) {
  for (auto& [param, arg] : subst->entries()) {
    CollectFreeTyvars(scope, arg, out_ftv);
  }
}


Type* Type::Generalize(Scope* scope, TypeArena* arena) {
  // collect tyvars at the specified scope
  TyvarCollection ftv;
  CollectFreeTyvars(scope, this, &ftv);

  // internal representation also
  for (size_t i = 0; i < ftv.vec.size(); ++i) {
    auto var = ftv.vec[i];

    for (auto ub : var->upper_bound()->elements()) {
      CollectFreeTyvars(scope, ub, &ftv);
    }
    for (auto lb : var->lower_bound()->elements()) {
      CollectFreeTyvars(scope, lb, &ftv);
    }
    if (var->func_shape() != nullptr) {
      CollectFreeTyvars(scope, var->func_shape(), &ftv);
    }
  }

  if (ftv.vec.size() == 0) {
    return this;
  } else {
    auto scheme = new TypeScheme(std::move(ftv.vec), this);
    arena->adopt_type(TypePtr(scheme));
    return scheme;
  }
}


static void NormalizeUpperBound(IntersectionType* upper_bound, Type* ub) {
  upper_bound->add_element(ub);

  if (ub->kind() == Type::Kind::kMetavar) {
    auto ub_metavar = ub->As<TypeMetavar>();
    for (auto ub_elem : ub_metavar->upper_bound()->elements()) {
      if (ub_elem->kind() != Type::Kind::kMetavar) {
        NormalizeUpperBound(upper_bound, ub_elem);
      }
    }
  }
}


static void NormalizeLowerBound(UnionType* lower_bound, Type* lb) {
  lower_bound->add_element(lb);

  if (lb->kind() == Type::Kind::kMetavar) {
    auto lb_metavar = lb->As<TypeMetavar>();
    for (auto lb_elem : lb_metavar->lower_bound()->elements()) {
      if (lb_elem->kind() != Type::Kind::kMetavar) {
        NormalizeLowerBound(lower_bound, lb_elem);
      }
    }
  }
}


Type* Type::Instantiate(TypeArena* arena, Substitution* parent) const {
  return const_cast<Type*>(this);
}


Type* TypeScheme::Instantiate(TypeArena* arena, Substitution* parent) const {
  InstantiatedInfoPtr instantiated_info = std::make_unique<InstantiatedInfo>();

  // prepare subst: scheme->vars() => fresh metavars
  Substitution subst(parent);
  for (auto tyvar : this->vars()) {
    auto metavar = new TypeMetavar();
    xylo_check(subst.Insert(tyvar, metavar));
    instantiated_info->vars.push_back(metavar);
    arena->adopt_type(TypePtr(metavar));
  }

  // restore bounds
  for (auto [tyvar, fresh] : subst.entries()) {
    auto metavar = fresh->As<TypeMetavar>();

    // substitute upper bounds
    auto upper_bound = new IntersectionType();
    for (auto t : tyvar->upper_bound()->elements()) {
      NormalizeUpperBound(upper_bound, subst.Apply(t, arena));
    }
    metavar->set_upper_bound(IntersectionTypePtr(upper_bound));

    // substitute lower bounds
    auto lower_bound = new UnionType();
    for (auto t : tyvar->lower_bound()->elements()) {
      NormalizeLowerBound(lower_bound, subst.Apply(t, arena));
    }
    metavar->set_lower_bound(UnionTypePtr(lower_bound));

    // substitute function shape
    if (tyvar->func_shape() != nullptr) {
      auto func_shape = subst.Apply(tyvar->func_shape(), arena);
      metavar->set_func_shape(func_shape->As<FunctionType>());
      metavar->set_upper_func_shape(tyvar->upper_func_shape());
      metavar->set_lower_func_shape(tyvar->lower_func_shape());
    }
  }

  auto result = subst.Apply(this->body(), arena);
  result->set_instantiated_info(std::move(instantiated_info));
  return result;
}


Type* TypeApplication::Instantiate(TypeArena* arena, Substitution* parent) const {
  xylo_contract(substitution()->parent() == nullptr);
  substitution()->set_parent(parent);
  Finally _([this]() {
    substitution()->set_parent(nullptr);
  });

  return target()->Instantiate(arena, substitution());
}


template <typename T>
static Type* ApplyPerElement(const Substitution* subst, Type* type, TypeArena* arena) {
  auto container = type->template As<T>();
  auto new_container = new T();

  bool changed = false;
  for (auto elem : container->elements()) {
    auto sbsted_elem = subst->Apply(elem, arena);
    new_container->add_element(sbsted_elem);
    changed |= (sbsted_elem != elem);
  }

  if (changed) {
    arena->adopt_type(TypePtr(new_container));
    return new_container;
  } else {
    delete new_container;
    return container;
  }
}


Type* Substitution::Apply(Type* type, TypeArena* arena, bool savable_this) const {
  switch (type->kind()) {
    case Type::Kind::kError:
      return type;

    case Type::Kind::kNominal: {
      auto nominal = type->As<NominalType>();
      if (nominal->scope() == nullptr || !nominal->scope()->is_inner_than_equal(this->scope_)) {
        return type;
      }

      // The nominal type is defined within this substitution's scope,
      // so we need to apply the substitution to its members.
      auto new_nominal = new NominalType(nominal->category(), nominal->name());
      new_nominal->set_scope(nominal->scope());
      new_nominal->set_origin(nominal);
      new_nominal->set_substitution(this->clone());
      arena->adopt_type(TypePtr(new_nominal));
      auto sbst = new_nominal->substitution();

      for (auto super : nominal->supers()) {
        auto sbsted_super = sbst->Apply(super, arena);
        new_nominal->AddSuper(sbsted_super->As<NominalType>());
      }

      for (auto embedded : nominal->embeddeds()) {
        auto sbsted_type = sbst->Apply(embedded->type(), arena);
        new_nominal->AddEmbedded(embedded->name(), sbsted_type->As<NominalType>());
      }

      for (auto field : nominal->fields()) {
        auto sbsted_type = sbst->Apply(field->type(), arena);
        new_nominal->AddField(field->name(), sbsted_type);
      }

      for (auto method : nominal->methods()) {
        auto sbsted_type = method->type() ? sbst->Apply(method->type(), arena, true) : nullptr;
        auto member_info = new_nominal->AddMethod(method->name(), sbsted_type);
        if (sbsted_type == nullptr) {
          member_info->set_type_resolver([=]() {
            method->resolve_type();
            member_info->set_type(sbst->Apply(method->type(), arena, true));
          });
        }
      }

      return new_nominal;
    }

    case Type::Kind::kMemberConstraint: {
      auto memcon = type->As<MemberConstraint>();
      auto expected_type = Apply(memcon->expected_type(), arena);
      if (expected_type == memcon->expected_type()) {
        return memcon;
      } else {
        auto new_memcon = new MemberConstraint(memcon->member_name(), expected_type);
        new_memcon->SetMutable(memcon->is_mutable());
        new_memcon->set_on_error(memcon->on_error());
        new_memcon->set_resolved_in(memcon->resolved_in());
        new_memcon->set_resolved_member(memcon->resolved_member());
        arena->adopt_type(TypePtr(new_memcon));
        return new_memcon;
      }
    }

    case Type::Kind::kFunction: {
      auto func = type->As<FunctionType>();
      auto sbsted_params = Apply(func->params_type(), arena);
      auto sbsted_return = Apply(func->return_type(), arena);

      if (sbsted_params == func->params_type() && sbsted_return == func->return_type()) {
        return func;
      } else {
        auto new_func = new FunctionType(func->is_closure(), sbsted_params->As<TupleType>(), sbsted_return);
        arena->adopt_type(TypePtr(new_func));
        return new_func;
      }
    }

    case Type::Kind::kTuple:
      return ApplyPerElement<TupleType>(this, type, arena);

    case Type::Kind::kIntersection:
      return ApplyPerElement<IntersectionType>(this, type, arena);

    case Type::Kind::kUnion:
      return ApplyPerElement<UnionType>(this, type, arena);

    case Type::Kind::kTyvar: {
      auto var = type->As<TypeVariable>();
      auto it = entries_.find(var);
      if ((it != entries_.end())) {
        return Apply(it->second, arena);
      } else if (parent_ != nullptr) {
        return parent_->Apply(var, arena);
      } else {
        return var;
      }
    }

    case Type::Kind::kMetavar:
      return type;

    case Type::Kind::kScheme:
    case Type::Kind::kApplication: {
      if (!savable_this) {
        xylo_unreachable();
      }

      // defer substitution until instantiation
      auto tyapp = new TypeApplication(type, const_cast<Substitution*>(this));
      arena->adopt_type(TypePtr(tyapp));
      return tyapp;
    }
  }

  xylo_unreachable();
}


Type* ErrorType::Zonk(const Substitution* subst, bool strict, TypeArena* arena) {
  return this;
}


Type* NominalType::Zonk(const Substitution* subst, bool strict, TypeArena* arena) {
  return this;
}


Type* MemberConstraint::Zonk(const Substitution* subst, bool strict, TypeArena* arena) {
  xylo_unreachable();
}


Type* FunctionType::Zonk(const Substitution* subst, bool strict, TypeArena* arena) {
  auto params_zonked = params_type_->Zonk(subst, strict, arena);
  auto return_zonked = return_type_->Zonk(subst, strict, arena);

  if (params_zonked == params_type_ && return_zonked == return_type_) {
    return this;
  }

  if (strict) {
    if (params_zonked->kind() == Type::Kind::kError || return_zonked->kind() == Type::Kind::kError) {
      return ErrorType::instance();
    }
  }

  auto zonked_func = new FunctionType(closure_, params_zonked->As<TupleType>(), return_zonked);
  arena->adopt_type(TypePtr(zonked_func));
  return zonked_func;
}


Type* TupleType::Zonk(const Substitution* subst, bool strict, TypeArena* arena) {
  auto zonked_tuple = new TupleType();

  bool changed = false;
  for (auto elem : elements_) {
    auto zonked_elem = elem->Zonk(subst, strict, arena);
    zonked_tuple->add_element(zonked_elem);
    changed |= (zonked_elem != elem);

    if (strict && zonked_elem->kind() == Type::Kind::kError) {
      delete zonked_tuple;
      return ErrorType::instance();
    }
  }

  if (changed) {
    arena->adopt_type(TypePtr(zonked_tuple));
    return zonked_tuple;
  } else {
    delete zonked_tuple;
    return this;
  }
}


Type* IntersectionType::Zonk(const Substitution* subst, bool strict, TypeArena* arena) {
  xylo_contract(is_top_type());  // now only top type is allowed
  return this;
}


Type* UnionType::Zonk(const Substitution* subst, bool strict, TypeArena* arena) {
  xylo_contract(is_bottom_type());  // now only bottom type is allowed
  return this;
}


Type* TypeVariable::Zonk(const Substitution* subst, bool strict, TypeArena* arena) {
  auto zonked = subst->Apply(this, arena);
  if (zonked != this) {
    return zonked->Zonk(subst, strict, arena);
  } else {
    return zonked;
  }
}


Type* TypeMetavar::Zonk(const Substitution* subst, bool strict, TypeArena* arena) {
  auto instantiated_info = this->instantiated_info();

  UnionType atoms;
  UnionType result;
  for (auto elem : lower_bound()->elements()) {
    switch (elem->kind()) {
      case Type::Kind::kNominal:
        atoms.add_element(elem);
        break;

      case Type::Kind::kTyvar: {
        auto zonked = elem->Zonk(subst, strict, arena);
        if (zonked->kind() == Type::Kind::kTyvar) {
          result.add_element(zonked);
        } else {
          atoms.add_element(zonked);
        }
        if (zonked->instantiated_info() != nullptr) {
          xylo_contract(instantiated_info == nullptr);
          instantiated_info = zonked->instantiated_info();
        }
        break;
      }

      case Type::Kind::kMetavar:
        break;

      default:
        xylo_unreachable();
    }
  }

  if (func_shape() != nullptr) {
    auto result = func_shape()->Zonk(subst, strict, arena);
    result->set_instantiated_info(instantiated_info);
    return result;
  }

  atoms.ShrinkToUpper();
  result.ShrinkToUpper();

  // zonked type is a common ancestor of the lower bound and is a subtype of the upper bound
  if (atoms.elements().size() > 0) {
    bool found = false;
    Vector<Type*> ancestors;
    FindCommonAncestors(&atoms, &ancestors);

    for (auto candidate : ancestors) {
      if (candidate->CanConstrainSubtypeOf(upper_bound())) {
        result.add_element(candidate);
        found = true;
      }
    }

    if (!found) {
      xylo_contract(ancestors.size() == 0);
      if (strict) {
        return ErrorType::instance();
      } else {
        auto top_type = new IntersectionType();
        arena->adopt_type(TypePtr(top_type));
        return top_type;
      }
    }
  }

  switch (result.elements().size()) {
    case 0: {
      if (strict) {
        return ErrorType::instance();
      } else {
        auto bottom_type = new UnionType();
        arena->adopt_type(TypePtr(bottom_type));
        return bottom_type;
      }
    }

    case 1:
      return result.elements()[0];

    default:
      if (strict) {
        return ErrorType::instance();
      } else {
        return result.elements()[0];
      }
  }
}


Type* TypeScheme::Zonk(const Substitution* subst, bool strict, TypeArena* arena) {
  xylo_unreachable();
}


Type* TypeApplication::Zonk(const Substitution* subst, bool strict, TypeArena* arena) {
  xylo_unreachable();
}


bool Type::is_monotype() const {
  switch (kind()) {
    case Type::Kind::kError:
      return true;

    case Type::Kind::kNominal:
      return true;

    case Type::Kind::kFunction: {
      auto func = As<FunctionType>();
      return func->params_type()->is_monotype() && func->return_type()->is_monotype();
    }

    case Type::Kind::kTuple: {
      auto tuple = As<TupleType>();
      for (auto elem : tuple->elements()) {
        if (!elem->is_monotype()) {
          return false;
        }
      }
      return true;
    }

    case Type::Kind::kMemberConstraint:
    case Type::Kind::kIntersection:
    case Type::Kind::kUnion:
    case Type::Kind::kTyvar:
    case Type::Kind::kMetavar:
    case Type::Kind::kScheme:
    case Type::Kind::kApplication:
      return false;
  }

  xylo_unreachable();
}


std::string TypePrinter::Print(const Type* type, bool paren, Status* status) const {
  switch (type->kind()) {
    case Type::Kind::kError:
      return "<Error>";

    case Type::Kind::kNominal:
      return Print(type->As<NominalType>(), paren, status);

    case Type::Kind::kMemberConstraint:
      return Print(type->As<MemberConstraint>(), paren, status);

    case Type::Kind::kFunction:
      return Print(type->As<FunctionType>(), paren, status);

    case Type::Kind::kTuple:
      return Print(type->As<TupleType>(), paren, status);

    case Type::Kind::kIntersection:
      return Print(type->As<IntersectionType>(), paren, status);

    case Type::Kind::kUnion:
      return Print(type->As<UnionType>(), paren, status);

    case Type::Kind::kTyvar:
      return Print(type->As<TypeVariable>(), paren, status);

    case Type::Kind::kMetavar:
      return Print(type->As<TypeMetavar>(), paren, status);

    case Type::Kind::kScheme:
      return Print(type->As<TypeScheme>(), paren, status);

    case Type::Kind::kApplication:
      return Print(type->As<TypeApplication>(), paren, status);
  }

  xylo_unreachable();
}


std::string TypePrinter::Print(const NominalType* type, bool paren, Status* status) const {
  std::string str = "";

  if (type->name()->str() != HString()) {
    str += type->name()->str().cpp_str();
  } else {
    str += "<" + std::to_string(reinterpret_cast<uintptr_t>(type)) + ">";
  }

  auto t = type;
  while (t != nullptr) {
    if (t->substitution() == nullptr) {
      break;
    }
    str += Print(t->substitution(), status);
    t = t->origin();
  }

  return str;
}


std::string TypePrinter::Print(const MemberConstraint* type, bool paren, Status* status) const {
  std::string str = "";

  if (!options_.verbose && type->is_resolved()) {
    return Print(type->resolved_in(), paren, status);
  }

  if (type->is_resolved()) {
    str += Print(type->resolved_in(), false, status);
    str += ".";
  }
  str += type->member_name()->str().cpp_str();
  str += ": " + Print(type->expected_type(), false, status);

  return "{" + str + "}";
}


std::string TypePrinter::Print(const FunctionType* type, bool paren, Status* status) const {
  std::string str = "";

  str += Print(type->params_type(), true, status);
  if (options_.verbose && type->is_closure()) {
    str += " => ";
  } else {
    str += " -> ";
  }
  if (type->return_type()->kind() == Type::Kind::kFunction ||
      (!options_.verbose && type->return_type()->is_function_type())) {
    str += Print(type->return_type(), false, status);
  } else {
    str += Print(type->return_type(), true, status);
  }

  if (paren) {
    return "(" + str + ")";
  } else {
    return str;
  }
}


std::string TypePrinter::Print(const TupleType* type, bool paren, Status* status) const {
  std::string str = "";

  for (auto elem : type->elements()) {
    if (!str.empty()) {
      str += ", ";
    }

    if (type->elements().size() == 1) {
      str += Print(elem, paren, status);
    } else {
      str += Print(elem, false, status);
    }
  }

  if (type->elements().size() == 1) {
    return str;
  } else {
    return "(" + str + ")";
  }
}


std::string TypePrinter::Print(const IntersectionType* type, bool paren, Status* status) const {
  if (type->empty()) {
    return "<Top>";
  }

  std::string str = "";

  for (auto elem : type->elements()) {
    if (!str.empty()) {
      str += " & ";
    }

    if (type->elements().size() == 1) {
      str += Print(elem, paren, status);
    } else {
      str += Print(elem, true, status);
    }
  }

  if (paren && type->elements().size() >= 2) {
    return "(" + str + ")";
  } else {
    return str;
  }
}


std::string TypePrinter::Print(const UnionType* type, bool paren, Status* status) const {
  if (type->empty()) {
    return "<Bottom>";
  }

  std::string str = "";

  for (auto elem : type->elements()) {
    if (!str.empty()) {
      str += " | ";
    }

    if (type->elements().size() == 1) {
      str += Print(elem, paren, status);
    } else {
      str += Print(elem, true, status);
    }
  }

  if (paren && type->elements().size() >= 2) {
    return "(" + str + ")";
  } else {
    return str;
  }
}

template <class T>
static Type* FindExactType(T* var) {
  Set<Type*> lower_set;
  for (auto lb : var->lower_bound()->elements()) {
    lower_set.emplace(lb);
  }

  for (auto ub : var->upper_bound()->elements()) {
    if (lower_set.contains(ub)) {
      // found exact type
      return ub;
    }
  }

  return nullptr;
}


template <class T, class O, class S, class F>
std::string PrintCommonVar(T type, bool paren, bool is_metavar, const O& opt, S* status, F print) {
  if (!opt.verbose) {
    if (type->func_shape() != nullptr) {
      return print(type->func_shape(), paren, status);
    }
  }

  {
    // alias?
    auto it = status->alias_map.find(type);
    if (it != status->alias_map.end()) {
      return print(it->second, paren, status);
    }
  }

  {
    // appeared var?
    auto it = status->name_map->find(type);
    if (it != status->name_map->end()) {
      return it->second;
    }
  }

  if (!opt.verbose) {
    auto exact_type = FindExactType(type);
    if (exact_type != nullptr) {
      if (exact_type->kind() == Type::Kind::kNominal || status->name_map->contains(exact_type)) {
        // nominal type or already named type
        status->alias_map.emplace(type, exact_type);
        return print(exact_type, paren, status);
      }
      status->alias_map.emplace(exact_type, type);
    }
  }

  auto n = status->name_map->size();
  // A, B, ..., R, S, T1, T2, ...
  auto name = (n < 19) ? std::string({static_cast<char>('A' + n)}) : "T" + std::to_string(n - 18);
  if (opt.verbose && is_metavar) {
    name += "'";
  }
  status->name_map->emplace(type, name);
  status->appeared.push_back(type);
  return name;
}


std::string TypePrinter::Print(const TypeVariable* type, bool paren, Status* status) const {
  auto str = PrintCommonVar(type, paren, false, options_, status, [this](auto t, bool p, Status* s) {
    return Print(t, p, s);
  });
  // str += std::to_string(type->scope()->depth());
  return str;
}


std::string TypePrinter::Print(const TypeMetavar* type, bool paren, Status* status) const {
  return PrintCommonVar(type, paren, true, options_, status, [this](auto t, bool p, Status* s) {
    return Print(t, p, s);
  });
}


std::string TypePrinter::Print(const TypeScheme* type, bool paren, Status* status) const {
  if (!options_.verbose) {
    return Print(type->body(), paren, status);
  }

  std::string str = "forall ";

  bool first = true;
  for (auto var : type->vars()) {
    if (!first) {
      str += " ";
    }
    first = false;
    str += Print(var, false, status);
  }

  str += ". ";
  str += Print(type->body(), false, status);

  return str;
}


std::string TypePrinter::Print(const TypeApplication* type, bool paren, Status* status) const {
  if (!options_.verbose) {
    for (auto& [param, arg] : type->substitution()->entries()) {
      status->alias_map.emplace(param, arg);
    }
    return Print(type->target(), paren, status);
  }

  std::string str = "";
  str += Print(type->substitution(), status);
  str += " ";
  str += Print(type->target(), true, status);

  return str;
}


std::string TypePrinter::Print(const Substitution* subst, Status* status) const {
  std::string str = "";
  str += "[";
  bool first = true;
  for (auto& [param, arg] : subst->entries()) {
    if (!first) {
      str += ", ";
    }
    first = false;
    str += Print(param, false, status);
    str += " := ";
    str += Print(arg, false, status);
  }
  str += "]";

  return str;
}


std::string TypePrinter::PrintConstraints(Status* status) const {
  std::string str = "";

  auto attach = [](const Type* t, auto f) {
    if (t->kind() == Type::Kind::kTyvar) {
      return f(t->As<TypeVariable>());
    } else if (t->kind() == Type::Kind::kMetavar) {
      return f(t->As<TypeMetavar>());
    } else {
      xylo_unreachable();
    }
  };

  auto empty_bound = [](auto var) {
    return var->lower_bound()->empty() && var->upper_bound()->empty() && var->func_shape() == nullptr;
  };

  if (options_.verbose) {
    // full detail

    auto print_bound = [this, &status](auto var) {
      std::string str = "";

      str += Print(var->lower_bound(), true, status);
      str += " <: ";
      str += Print(var, false, status);
      if (var->func_shape() != nullptr) {
        str += " = ";
        str += Print(var->func_shape(), true, status);
      }
      str += " <: ";
      str += Print(var->upper_bound(), true, status);

      return str;
    };

    for (size_t i = 0; i < status->appeared.size(); ++i) {
      auto var = status->appeared[i];
      if (attach(var, empty_bound)) {
        continue;
      }
      if (!str.empty()) {
        str += ", ";
      }
      str += attach(var, print_bound);
    }

  } else {
    // simplify

    // binary relationship
    struct BinaryRelation {
      const Type* lower;
      const Type* upper;
      bool always_print;

      BinaryRelation(const Type* lower, const Type* upper) :
          lower(lower),
          upper(upper),
          always_print(false) {}
    };
    Map<std::pair<const Type*, const Type*>, BinaryRelation, PairHash> relations;

    // simplified bounds
    struct Bound {
      UnionTypePtr lowers;
      IntersectionTypePtr uppers;
    };
    Map<const Type*, Bound> simplified_bounds;

    auto collect_relation = [this, &status, &relations, &simplified_bounds](auto var) {
      Bound bound = {std::make_unique<UnionType>(), std::make_unique<IntersectionType>()};

      // collect aliased bounds
      for (const auto* lb : var->lower_bound()->elements()) {
        auto it = status->alias_map.find(lb);
        if (it != status->alias_map.end()) {
          lb = it->second;
        }
        if (lb == var) {
          continue;
        }
        bound.lowers->add_element(const_cast<Type*>(lb));
      }
      for (const auto* ub : var->upper_bound()->elements()) {
        auto it = status->alias_map.find(ub);
        if (it != status->alias_map.end()) {
          ub = it->second;
        }
        if (ub->kind() == Type::Kind::kMemberConstraint) {
          auto memcon = ub->template As<MemberConstraint>();
          if (memcon->is_resolved()) {
            ub = memcon->resolved_in();
          }
        }
        if (ub == var) {
          continue;
        }
        bound.uppers->add_element(const_cast<Type*>(ub));
      }

      bound.lowers->ShrinkToUpper();
      bound.uppers->ShrinkToLower();

      auto multiple_lower = bound.lowers->elements().size() > 1;
      auto multiple_upper = bound.uppers->elements().size() > 1;

      // register relations
      for (auto lb : bound.lowers->elements()) {
        Print(lb, false, status);  // naming only
        auto [it, _] = relations.emplace(std::make_pair(lb, var), BinaryRelation(lb, var));
        it->second.always_print |= multiple_lower;
      }
      for (auto ub : bound.uppers->elements()) {
        Print(ub, false, status);  // naming only
        auto [it, _] = relations.emplace(std::make_pair(var, ub), BinaryRelation(var, ub));
        it->second.always_print |= multiple_upper;
      }

      simplified_bounds.emplace(var, std::move(bound));
    };

    auto print_bound = [this, &status, &relations, &simplified_bounds](auto var) {
      std::string lower_str = "";
      std::string upper_str = "";
      auto& bound = simplified_bounds.at(var);

      if (bound.lowers->elements().size() > 1) {
        // multiple lowers: always print
        lower_str += Print(bound.lowers.get(), true, status);
        lower_str += " <: ";
      }
      if (bound.lowers->elements().size() == 1) {
        // single lower: print only if not printed yet
        auto it = relations.find(std::make_pair(bound.lowers->elements()[0], var));
        xylo_contract(it != relations.end());
        if (!it->second.always_print) {
          it->second.always_print = true;
          lower_str += Print(bound.lowers.get(), true, status);
          lower_str += " <: ";
        }
      }

      if (bound.uppers->elements().size() > 1) {
        // multiple uppers: always print
        upper_str += " <: ";
        upper_str += Print(bound.uppers.get(), true, status);
      }
      if (bound.uppers->elements().size() == 1) {
        // single upper: print only if not printed yet
        auto it = relations.find(std::make_pair(var, bound.uppers->elements()[0]));
        xylo_contract(it != relations.end());
        if (!it->second.always_print) {
          it->second.always_print = true;
          upper_str += " <: ";
          upper_str += Print(bound.uppers.get(), true, status);
        }
      }

      if (lower_str.empty() && upper_str.empty()) {
        return std::string();
      } else {
        return lower_str + Print(var, false, status) + upper_str;
      }
    };

    // collect relations
    for (size_t i = 0; i < status->appeared.size(); ++i) {
      auto var = status->appeared[i];
      attach(var, collect_relation);
    }

    // print only necessary ones
    for (auto var : status->appeared) {
      auto var_str = attach(var, print_bound);
      if (!var_str.empty()) {
        if (!str.empty()) {
          str += ", ";
        }
        str += var_str;
      }
    }
  }

  if (str.empty()) {
    return str;
  } else {
    return " where " + str;
  }
}


}  // namespace xylo
