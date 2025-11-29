
#include "xylo/syntax/type.h"

#include <functional>

#include "xylo/syntax/substitution.h"
#include "xylo/util/finally.h"

namespace xylo {


MemberInfo* NominalType::GetMember(Identifier* member_name) const {
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

  for (auto super : supers()) {
    auto member = super->GetMember(member_name);
    if (member != nullptr) {
      return member;
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


bool NominalType::GetMemberPath(Identifier* member_name, Vector<MemberInfo*>* out_path) const {
  {
    auto member = GetDeclaredMember(member_name);
    if (member != nullptr) {
      out_path->push_back(member);
      return true;
    }
  }

  for (auto embedding : embeddings_) {
    auto embed_type = embedding->type()->As<NominalType>();
    if (embed_type->GetMemberPath(member_name, out_path)) {
      out_path->push_back(embedding);
      return true;
    }
  }

  // TODO: supers

  return false;
}


void NominalType::FindEmbededMembers(Identifier* member_name, Vector<MemberInfo*>* out_members) const {
  for (auto embedding : embeddings_) {
    auto embed_type = embedding->type()->As<NominalType>();
    auto member = embed_type->GetDeclaredMember(member_name);
    if (member != nullptr) {
      out_members->push_back(member);
    }
    embed_type->FindEmbededMembers(member_name, out_members);
  }
}


bool NominalType::HasEmbedding(NominalType* clazz) const {
  if (this == clazz) {
    return true;
  }

  for (auto embedding : embeddings_) {
    if (embedding->type()->As<NominalType>()->HasEmbedding(clazz)) {
      return true;
    }
  }

#if 0
  for (auto super : supers_) {
    if (super->HasEmbedding(clazz)) {
      return true;
    }
  }
#endif

  return false;
}


MemberInfo* NominalType::AddField(Identifier* field_name, Type* field_type) {
  if (GetDeclaredMember(field_name) != nullptr) {
    return nullptr;
  }

  int index = fields_.size();
  auto field = new MemberInfo(this, MemberInfo::Kind::kField, index, field_name, field_type);

  fields_.push_back(field);
  member_map_.emplace(field_name, MemberInfoPtr(field));
  return field;
}


MemberInfo* NominalType::AddEmbedding(Identifier* field_name, NominalType* clazz) {
  if (GetDeclaredMember(field_name) != nullptr) {
    return nullptr;
  }

  // embedding is also a field
  int index = fields_.size();
  auto embedding = new MemberInfo(this, MemberInfo::Kind::kEmbedding, index, field_name, clazz);

  fields_.push_back(embedding);
  embeddings_.push_back(embedding);
  member_map_.emplace(field_name, MemberInfoPtr(embedding));
  return embedding;
}


MemberInfo* NominalType::AddMethod(Identifier* method_name, Type* method_type) {
  if (GetDeclaredMember(method_name) != nullptr) {
    return nullptr;
  }

  int index = methods_.size();
  auto method = new MemberInfo(this, MemberInfo::Kind::kMethod, index, method_name, method_type);

  methods_.push_back(method);
  member_map_.emplace(method_name, MemberInfoPtr(method));
  return method;
}


bool ErrorType::equals(const Type* other) const {
  return true;
}


bool NominalType::equals(const Type* other) const {
  return this == other;
}


bool MemberRequirement::equals(const Type* other) const {
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


template <typename F1, typename F2, typename F3, typename F4, typename F5, typename F6, typename F7, typename F8>
struct SubtypingProc {
  F1 recurse;
  F2 onRecToMemberReq;
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

  // nominal to member requirement
  if (src->kind() == Type::Kind::kNominal && dst->kind() == Type::Kind::kMemberReq) {
    auto src_nominal = src->template As<NominalType>();
    auto dst_memreq = dst->template As<MemberRequirement>();
    if (dst_memreq->is_resolved()) {
      return src_nominal->IsSubtypeOf(dst_memreq->resolved()->owner());
    }
    return proc.onRecToMemberReq(src_nominal, dst_memreq);
  }

  // member requirement src is only compatible with nominal dst if resolved
  if (src->kind() == Type::Kind::kMemberReq) {
    if (dst->kind() == Type::Kind::kNominal) {
      auto src_memreq = src->template As<MemberRequirement>();
      if (src_memreq->is_resolved()) {
        return src_memreq->resolved()->owner()->IsSubtypeOf(dst);
      }
    }
    return false;
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

    .onRecToMemberReq =
        [=](const NominalType* nominal, const MemberRequirement* memreq) {
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

    .onRecToMemberReq =
        [=](const NominalType* nominal, const MemberRequirement* memreq) {
          return memreq->CanAccept(nominal, visited);
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

    .onRecToMemberReq =
        [=](NominalType* nominal, MemberRequirement* memreq) {
          return memreq->Accept(nominal, visited);
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


bool MemberRequirement::CanAccept(const NominalType* nominal, TypePairSet* visited) const {
  auto member = nominal->GetMember(this->name());
  if (member == nullptr) {
    CallbackOnError(nominal);
    return false;
  }

  if (member->type() == nullptr) {
    member->resolve_type();
    xylo_contract(member->type() != nullptr);
  }

  if (is_mutable() && member->kind() != MemberInfo::Kind::kField) {
    CallbackOnError(nominal);
    return false;
  }

  TypeSink allocated;
  Vector<TypeMetavar*> instantiated_vars;
  auto member_type = member->type()->Instantiate(&allocated, &instantiated_vars);

  if (!member_type->CanConstrainSubtypeOf(this->type(), visited)) {
    CallbackOnError(nominal);
    return false;
  }
  if (!this->type()->CanConstrainSubtypeOf(member_type, visited)) {
    CallbackOnError(nominal);
    return false;
  }

  return true;
}


bool MemberRequirement::Accept(NominalType* nominal, TypePairSet* visited) {
  auto member = nominal->GetMember(this->name());
  if (member == nullptr) {
    CallbackOnError(nominal);
    return false;
  }

  if (member->type() == nullptr) {
    member->resolve_type();
    xylo_contract(member->type() != nullptr);
  }

  if (is_mutable() && member->kind() != MemberInfo::Kind::kField) {
    CallbackOnError(nominal);
    return false;
  }

  TypeSink allocated;
  Vector<TypeMetavar*> instantiated_vars;
  auto member_type = member->type()->Instantiate(&allocated, &instantiated_vars);

  if (!member_type->ConstrainSameAs(this->type())) {
    CallbackOnError(nominal);
    return false;
  }

  this->set_resolved(member);
  this->set_instantiated_vars(std::move(instantiated_vars));
  this->type()->adopt_types(&allocated);

  return true;
}


bool MemberRequirement::SetMutable(bool m) {
  if (m && is_resolved() && resolved()->kind() != MemberInfo::Kind::kField) {
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

    case Type::Kind::kMemberReq:
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

    case Type::Kind::kMemberReq:
    case Type::Kind::kFunction:
    case Type::Kind::kTuple:
    case Type::Kind::kUnion:
    case Type::Kind::kScheme:
      xylo_unreachable();
  }
}


bool VariableBase::CanConstrainUpperBound(const Type* new_ub, TypePairSet* visited) const {
  xylo_contract(new_ub->kind() != Type::Kind::kTuple);
  xylo_contract(new_ub->kind() != Type::Kind::kIntersection);
  xylo_contract(new_ub->kind() != Type::Kind::kScheme);
  xylo_contract(!(owner_->kind() == Type::Kind::kTyvar && new_ub->kind() == Type::Kind::kMetavar));

  if (upper_bound()->IsSubtypeOf(new_ub)) {
    return true;
  }

  // circular types are prohibited
  if (OccursIn(new_ub, owner_, false)) {
    return false;
  }

  if (!lower_bound()->CanConstrainSubtypeOf(new_ub, visited)) {
    return false;
  }

  if (func_shape() != nullptr) {
    auto func_for_check = func_shape();
    FunctionType temp_func_shape(false, func_shape()->params_type(), func_shape()->return_type());
    if (!lower_based_closure_) {
      func_for_check = &temp_func_shape;
    }

    if (!func_for_check->CanConstrainSubtypeOf(new_ub, visited)) {
      return false;
    }
  }

  return true;
}


bool VariableBase::CanConstrainLowerBound(const Type* new_lb, TypePairSet* visited) const {
  xylo_contract(new_lb->kind() != Type::Kind::kMemberReq);
  xylo_contract(new_lb->kind() != Type::Kind::kTuple);
  xylo_contract(new_lb->kind() != Type::Kind::kUnion);
  xylo_contract(new_lb->kind() != Type::Kind::kScheme);
  xylo_contract(!(owner_->kind() == Type::Kind::kTyvar && new_lb->kind() == Type::Kind::kMetavar));

  if (new_lb->IsSubtypeOf(lower_bound())) {
    return true;
  }

  // circular types are prohibited
  if (OccursIn(new_lb, owner_, false)) {
    return false;
  }

  if (!new_lb->CanConstrainSubtypeOf(upper_bound(), visited)) {
    return false;
  }

  if (func_shape() != nullptr) {
    auto func_for_check = func_shape();
    FunctionType temp_func_shape(true, func_shape()->params_type(), func_shape()->return_type());
    if (lower_based_closure_) {
      func_for_check = &temp_func_shape;
    }

    if (!new_lb->CanConstrainSubtypeOf(func_for_check, visited)) {
      return false;
    }
  }

  return true;
}


bool VariableBase::ConstrainUpperBound(Type* new_ub, TypePairSet* visited) {
  xylo_contract(new_ub->kind() != Type::Kind::kTuple);
  xylo_contract(new_ub->kind() != Type::Kind::kIntersection);
  xylo_contract(new_ub->kind() != Type::Kind::kScheme);
  xylo_contract(!(owner_->kind() == Type::Kind::kTyvar && new_ub->kind() == Type::Kind::kMetavar));

  // avoid redundant constraints
  if (upper_bound()->IsSubtypeOf(new_ub)) {
    return true;
  }

  // new_ub must be a supertype of lower bound
  if (!lower_bound_->ConstrainSubtypeOf(new_ub, visited)) {
    return false;
  }

  // functionize if needed
  if (new_ub->kind() == Type::Kind::kFunction) {
    Functionize(new_ub->As<FunctionType>(), false);
    ConstrainUpperBoundForFunc(new_ub, visited);
  }

  // constrain func_shape if functionized
  if (func_shape() != nullptr) {
    if (!func_shape_->ConstrainSubtypeOf(new_ub, visited)) {
      return false;
    }
  }

  if (new_ub->kind() != Type::Kind::kFunction) {
    ConstrainUpperBoundForAtom(new_ub, visited);
  }

  return true;
}


bool VariableBase::ConstrainUpperBoundForFunc(Type* new_ub, TypePairSet* visited) {
  auto new_ub_func = new_ub->As<FunctionType>();

  // closure subtyping rules
  if (!lower_based_closure_ && func_shape()->is_closure()) {
    func_shape_->set_closure(new_ub_func->is_closure());
  } else if (lower_based_closure_ && !func_shape()->is_closure() && !new_ub_func->is_closure()) {
    lower_based_closure_ = false;
  }

  return true;
}


bool VariableBase::ConstrainUpperBoundForAtom(Type* new_ub, TypePairSet* visited) {
  if (new_ub->kind() == Type::Kind::kMemberReq) {
    auto ub_memreq = new_ub->As<MemberRequirement>();

    if (owner_->kind() == Type::Kind::kTyvar) {
      auto owner_tyvar = owner_->As<TypeVariable>();

      if (ub_memreq->type()->kind() == Type::Kind::kTyvar) {
        auto ub_member_tyvar = ub_memreq->type()->As<TypeVariable>();
        if (ub_member_tyvar->depth() > owner_tyvar->depth()) {
          ub_member_tyvar->set_depth(owner_tyvar->depth());
        }
      }
      if (ub_memreq->type()->kind() == Type::Kind::kMetavar) {
        auto ub_member_tyvar = new TypeVariable(owner_tyvar->depth());
        ub_member_tyvar->ConstrainSameAs(ub_memreq->type());
        ub_memreq->set_type(ub_member_tyvar);
        owner_->adopt_type(TypePtr(ub_member_tyvar));
      }
    }
  }

  upper_bound_->add_element(new_ub);
  return true;
}


bool VariableBase::ConstrainLowerBound(Type* new_lb, TypePairSet* visited) {
  xylo_contract(new_lb->kind() != Type::Kind::kMemberReq);
  xylo_contract(new_lb->kind() != Type::Kind::kTuple);
  xylo_contract(new_lb->kind() != Type::Kind::kUnion);
  xylo_contract(new_lb->kind() != Type::Kind::kScheme);
  xylo_contract(!(owner_->kind() == Type::Kind::kTyvar && new_lb->kind() == Type::Kind::kMetavar));

  // avoid redundant constraints
  if (new_lb->IsSubtypeOf(lower_bound())) {
    return true;
  }

  // new_lb must be a subtype of upper bound
  if (!new_lb->ConstrainSubtypeOf(upper_bound_.get(), visited)) {
    return false;
  }

  // functionize if needed
  if (new_lb->kind() == Type::Kind::kFunction) {
    Functionize(new_lb->As<FunctionType>(), true);
    ConstrainLowerBoundForFunc(new_lb, visited);
  }

  // constrain func_shape if functionized
  if (func_shape() != nullptr) {
    if (!new_lb->ConstrainSubtypeOf(func_shape_, visited)) {
      return false;
    }
  }

  if (new_lb->kind() != Type::Kind::kFunction) {
    ConstrainLowerBoundForAtom(new_lb, visited);
  }

  return true;
}


bool VariableBase::ConstrainLowerBoundForFunc(Type* new_lb, TypePairSet* visited) {
  auto new_lb_func = new_lb->As<FunctionType>();

  // closure subtyping rules
  if (!lower_based_closure_ && func_shape()->is_closure()) {
    func_shape_->set_closure(new_lb_func->is_closure());
    lower_based_closure_ = true;
  } else if (lower_based_closure_ && !func_shape()->is_closure()) {
    func_shape_->set_closure(new_lb_func->is_closure());
    lower_based_closure_ = true;
  }

  return true;
}


bool VariableBase::ConstrainLowerBoundForAtom(Type* new_lb, TypePairSet* visited) {
  // Add new_lb's ancestors to the upper bound to enable sibling checking for types added to lb in the future.
  auto ancestors = std::make_unique<UnionType>();
  Vector<Type*> out_allocated;
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
      owner_->adopt_type(std::move(ancestors));
      break;
  }

  lower_bound_->add_element(new_lb);
  return true;
}


void VariableBase::Functionize(const FunctionType* base_shape, bool lower_based) {
  if (func_shape_ != nullptr) {
    return;
  }

  auto params_size = base_shape->params_type()->elements().size();
  auto params = new TupleType();
  owner_->adopt_type(TypePtr(params));

  for (size_t i = 0; i < params_size; ++i) {
    auto var = func_rettype_();  // TODO: use TypeMetavar
    owner_->adopt_type(TypePtr(var));
    params->add_element(var);
  }

  auto return_type = func_rettype_();
  func_shape_ = new FunctionType(base_shape->is_closure(), params, return_type);
  owner_->adopt_type(TypePtr(return_type));
  owner_->adopt_type(TypePtr(func_shape_));

  lower_based_closure_ = lower_based;
}


bool VariableBase::ConstrainBothBounds(VariableBase* dst, TypePairSet* visited) {
  auto src = this;

  // Merged ConstrainLowerBound and ConstrainUpperBound

  if (src->owner_->IsSubtypeOf(dst->owner_)) {
    return true;
  }

  // xylo_check(src->lower_bound_->ConstrainSubtypeOf(dst->owner_, visited));
  for (auto src_lb : src->lower_bound()->elements()) {
    if (src_lb->kind() != dst->owner_->kind()) {
      if (!src_lb->ConstrainSubtypeOf(dst->owner_, visited)) {
        return false;
      }
    }
  }

  // xylo_check(src->owner_->ConstrainSubtypeOf(dst->upper_bound_.get(), visited));
  for (auto dst_ub : dst->upper_bound()->elements()) {
    if (dst_ub->kind() != src->owner_->kind()) {
      if (!src->owner_->ConstrainSubtypeOf(dst_ub, visited)) {
        return false;
      }
    }
  }

  // constrain func_shape if functionized
  if (src->func_shape() != nullptr) {
    if (!src->func_shape_->ConstrainSubtypeOf(dst->owner_, visited)) {
      return false;
    }
  }
  if (dst->func_shape() != nullptr) {
    if (!src->owner_->ConstrainSubtypeOf(dst->func_shape_, visited)) {
      return false;
    }
  }

  if (!src->ConstrainUpperBoundForAtom(dst->owner_, visited)) {
    return false;
  }
  if (!dst->ConstrainLowerBoundForAtom(src->owner_, visited)) {
    return false;
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


Type* ErrorType::CloseOverMetavars(int depth, TypeSink* out_allocated) {
  return this;
}


Type* NominalType::CloseOverMetavars(int depth, TypeSink* out_allocated) {
  return this;
}


Type* MemberRequirement::CloseOverMetavars(int depth, TypeSink* out_allocated) {
  return this;
}


Type* FunctionType::CloseOverMetavars(int depth, TypeSink* out_allocated) {
  params_type()->CloseOverMetavars(depth, out_allocated);
  return_type_ = return_type()->CloseOverMetavars(depth, out_allocated);
  return this;
}


Type* TupleType::CloseOverMetavars(int depth, TypeSink* out_allocated) {
  Vector<Type*> new_elements;
  for (auto elem : elements()) {
    auto new_elem = elem->CloseOverMetavars(depth, out_allocated);
    new_elements.push_back(new_elem);
  }

  elements_ = std::move(new_elements);
  return this;
}


Type* IntersectionType::CloseOverMetavars(int depth, TypeSink* out_allocated) {
  Vector<Type*> new_elements;
  for (auto elem : elements()) {
    auto new_elem = elem->CloseOverMetavars(depth, out_allocated);
    new_elements.push_back(new_elem);
  }

  elements_ = std::move(new_elements);
  return this;
}


Type* UnionType::CloseOverMetavars(int depth, TypeSink* out_allocated) {
  Vector<Type*> new_elements;
  for (auto elem : elements()) {
    auto new_elem = elem->CloseOverMetavars(depth, out_allocated);
    new_elements.push_back(new_elem);
  }

  elements_ = std::move(new_elements);
  return this;
}


Type* TypeVariable::CloseOverMetavars(int depth, TypeSink* out_allocated) {
  return this;
}


Type* TypeMetavar::CloseOverMetavars(int depth, TypeSink* out_allocated) {
  Substitution subst;
  auto outgoing = this->Zonk(&subst, true, out_allocated);
  if (outgoing->kind() == Type::Kind::kError) {
    // The metavar cannot be determined uniquely, so it should be made into a variable.
    outgoing = new TypeVariable(depth);
    this->ConstrainSameAs(outgoing);
  }
  return outgoing;
}


Type* TypeScheme::CloseOverMetavars(int depth, TypeSink* out_allocated) {
  return this;
}


void Type::PruneInnerScopeVars(int depth) {
  switch (kind()) {
    case Type::Kind::kError:
      return;

    case Type::Kind::kNominal:
      return;

    case Type::Kind::kMemberReq:
      return;

    case Type::Kind::kFunction: {
      auto func = As<FunctionType>();
      func->params_type()->PruneInnerScopeVars(depth);
      func->return_type()->PruneInnerScopeVars(depth);
      return;
    }

    case Type::Kind::kTuple: {
      auto tuple = As<TupleType>();
      for (auto elem : tuple->elements()) {
        elem->PruneInnerScopeVars(depth);
      }
      return;
    }

    case Type::Kind::kIntersection: {
      auto inter = As<IntersectionType>();
      for (auto elem : inter->elements()) {
        elem->PruneInnerScopeVars(depth);
      }
      return;
    }

    case Type::Kind::kUnion: {
      auto uni = As<UnionType>();
      for (auto elem : uni->elements()) {
        elem->PruneInnerScopeVars(depth);
      }
      return;
    }

    case Type::Kind::kTyvar: {
      auto tyvar = As<TypeVariable>();
      if (tyvar->depth() == depth) {
        tyvar->PruneInnerScopeVars();
      }
      return;
    }

    case Type::Kind::kMetavar:
      return;

    case Type::Kind::kScheme:
      return;
  }
}


void TypeVariable::PruneInnerScopeVars() {
  auto new_upper_bound = std::make_unique<IntersectionType>();
  auto new_lower_bound = std::make_unique<UnionType>();

  for (auto ub : upper_bound()->elements()) {
    if (ub->kind() == Type::Kind::kTyvar) {
      auto ub_tyvar = ub->As<TypeVariable>();
      if (ub_tyvar->depth() > depth()) {
        continue;
      }
    }
    new_upper_bound->add_element(ub);
  }

  for (auto lb : lower_bound()->elements()) {
    if (lb->kind() == Type::Kind::kTyvar) {
      auto lb_tyvar = lb->As<TypeVariable>();
      if (lb_tyvar->depth() > depth()) {
        continue;
      }
    }
    new_lower_bound->add_element(lb);
  }

  varbase_.set_upper_bound(std::move(new_upper_bound));
  varbase_.set_lower_bound(std::move(new_lower_bound));
}


static void CollectFreeTyVars(int depth, Type* type, Set<TypeVariable*>* ftv_set, Vector<TypeVariable*>* ftv_vec) {
  switch (type->kind()) {
    case Type::Kind::kError:
      return;

    case Type::Kind::kNominal:
      return;

    case Type::Kind::kMemberReq: {
      auto memreq = type->As<MemberRequirement>();
      CollectFreeTyVars(depth, memreq->type(), ftv_set, ftv_vec);
      return;
    }

    case Type::Kind::kFunction: {
      auto func = type->As<FunctionType>();
      CollectFreeTyVars(depth, func->params_type(), ftv_set, ftv_vec);
      CollectFreeTyVars(depth, func->return_type(), ftv_set, ftv_vec);
      return;
    }

    case Type::Kind::kTuple: {
      auto tuple = type->As<TupleType>();
      for (auto elem : tuple->elements()) {
        CollectFreeTyVars(depth, elem, ftv_set, ftv_vec);
      }
      return;
    }

    case Type::Kind::kIntersection: {
      auto inter = type->As<IntersectionType>();
      for (auto elem : inter->elements()) {
        CollectFreeTyVars(depth, elem, ftv_set, ftv_vec);
      }
      return;
    }

    case Type::Kind::kUnion: {
      auto uni = type->As<UnionType>();
      for (auto elem : uni->elements()) {
        CollectFreeTyVars(depth, elem, ftv_set, ftv_vec);
      }
      return;
    }

    case Type::Kind::kTyvar: {
      auto tyvar = type->As<TypeVariable>();
      if (tyvar->depth() < depth) {
        return;
      }

      if (ftv_set->emplace(tyvar).second) {
        ftv_vec->push_back(tyvar);
      }

      return;
    }

    case Type::Kind::kMetavar:
      return;

    case Type::Kind::kScheme:
      return;
  }
}


Type* Type::Generalize(int depth, TypeSink* out_allocated) {
  // collect tyvars at the specified depth
  Set<TypeVariable*> ftv_set;
  Vector<TypeVariable*> ftv_vec;
  CollectFreeTyVars(depth, this, &ftv_set, &ftv_vec);

  // internal representation also
  for (size_t i = 0; i < ftv_vec.size(); ++i) {
    auto var = ftv_vec[i];

    for (auto ub : var->upper_bound()->elements()) {
      CollectFreeTyVars(depth, ub, &ftv_set, &ftv_vec);
    }
    for (auto lb : var->lower_bound()->elements()) {
      CollectFreeTyVars(depth, lb, &ftv_set, &ftv_vec);
    }
    if (var->func_shape() != nullptr) {
      CollectFreeTyVars(depth, var->func_shape(), &ftv_set, &ftv_vec);
    }
  }

  if (ftv_vec.size() == 0) {
    return this;
  } else {
    auto scheme = new TypeScheme(std::move(ftv_vec), this);
    out_allocated->adopt_type(TypePtr(scheme));
    return scheme;
  }
}


Type* Type::Instantiate(TypeSink* out_allocated, Vector<TypeMetavar*>* out_instantiated_vars) const {
  return const_cast<Type*>(this);
}


Type* TypeScheme::Instantiate(TypeSink* out_allocated, Vector<TypeMetavar*>* out_instantiated_vars) const {
  // prepare subst: scheme->vars() => fresh metavars
  Substitution subst;
  for (auto tyvar : this->vars()) {
    auto metavar = new TypeMetavar();
    subst.insert(tyvar, metavar);
    out_instantiated_vars->push_back(metavar);
    out_allocated->adopt_type(TypePtr(metavar));
  }

  // restore bounds
  for (auto [tyvar, fresh] : subst.entries()) {
    auto metavar = fresh->As<TypeMetavar>();

    // substitute upper bounds
    auto upper_bound = new IntersectionType();
    for (auto t : tyvar->upper_bound()->elements()) {
      upper_bound->add_element(subst.Apply(t, out_allocated));
    }
    metavar->set_upper_bound(IntersectionTypePtr(upper_bound));

    // substitute lower bounds
    auto lower_bound = new UnionType();
    for (auto t : tyvar->lower_bound()->elements()) {
      lower_bound->add_element(subst.Apply(t, out_allocated));
    }
    metavar->set_lower_bound(UnionTypePtr(lower_bound));

    // substitute function shape
    if (tyvar->func_shape() != nullptr) {
      auto func_shape = subst.Apply(tyvar->func_shape(), out_allocated);
      metavar->set_func_shape(func_shape->As<FunctionType>(), tyvar->is_lower_based_closure());
    }
  }

  return subst.Apply(this->body(), out_allocated);
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


Type* ErrorType::Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) {
  return this;
}


Type* NominalType::Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) {
  return this;
}


Type* MemberRequirement::Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) {
  xylo_unreachable();
}


Type* FunctionType::Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) {
  auto params_zonked = params_type_->Zonk(env, strict, out_allocated);
  auto return_zonked = return_type_->Zonk(env, strict, out_allocated);

  if (params_zonked == params_type_ && return_zonked == return_type_) {
    return this;
  }

  if (strict) {
    if (params_zonked->kind() == Type::Kind::kError || return_zonked->kind() == Type::Kind::kError) {
      return ErrorType::instance();
    }
  }

  auto zonked_func = new FunctionType(closure_, params_zonked->As<TupleType>(), return_zonked);
  out_allocated->adopt_type(TypePtr(zonked_func));
  return zonked_func;
}


Type* TupleType::Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) {
  auto zonked_tuple = new TupleType();

  bool changed = false;
  for (auto elem : elements_) {
    auto zonked_elem = elem->Zonk(env, strict, out_allocated);
    zonked_tuple->add_element(zonked_elem);
    changed |= (zonked_elem != elem);

    if (strict && zonked_elem->kind() == Type::Kind::kError) {
      delete zonked_tuple;
      return ErrorType::instance();
    }
  }

  if (changed) {
    out_allocated->adopt_type(TypePtr(zonked_tuple));
    return zonked_tuple;
  } else {
    delete zonked_tuple;
    return this;
  }
}


Type* IntersectionType::Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) {
  throw std::logic_error("not implemented: IntersectionType::Zonk");
}


Type* UnionType::Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) {
  throw std::logic_error("not implemented: UnionType::Zonk");
}


Type* TypeVariable::Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) {
  return env->Apply(this, out_allocated);
}


Type* TypeMetavar::Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) {
  if (func_shape() != nullptr) {
    return func_shape()->Zonk(env, strict, out_allocated);
  }

  UnionType atoms;
  UnionType result;
  for (auto elem : lower_bound()->elements()) {
    switch (elem->kind()) {
      case Type::Kind::kNominal:
        atoms.add_element(elem);
        break;

      case Type::Kind::kTyvar: {
        auto zonked = elem->Zonk(env, strict, out_allocated);
        if (zonked->kind() == Type::Kind::kTyvar) {
          result.add_element(zonked);
        } else {
          atoms.add_element(zonked);
        }
        break;
      }

      case Type::Kind::kMetavar:
        break;

      default:
        xylo_unreachable();
    }
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
      if (strict) {
        return ErrorType::instance();
      } else {
        auto top_type = new IntersectionType();
        out_allocated->adopt_type(TypePtr(top_type));
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
        out_allocated->adopt_type(TypePtr(bottom_type));
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


Type* TypeScheme::Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) {
  xylo_unreachable();
}


bool Type::IsGroundType() const {
  switch (kind()) {
    case Type::Kind::kError:
      return true;

    case Type::Kind::kNominal:
      return true;

    case Type::Kind::kFunction: {
      auto func = As<FunctionType>();
      return func->params_type()->IsGroundType() && func->return_type()->IsGroundType();
    }

    case Type::Kind::kTuple: {
      auto tuple = As<TupleType>();
      for (auto elem : tuple->elements()) {
        if (!elem->IsGroundType()) {
          return false;
        }
      }
      return true;
    }

    case Type::Kind::kMemberReq:
    case Type::Kind::kIntersection:
    case Type::Kind::kUnion:
    case Type::Kind::kTyvar:
    case Type::Kind::kMetavar:
    case Type::Kind::kScheme:
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

    case Type::Kind::kMemberReq:
      return Print(type->As<MemberRequirement>(), paren, status);

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
  }

  xylo_unreachable();
}


std::string TypePrinter::Print(const NominalType* type, bool paren, Status* status) const {
  if (type->name()->str() != HString()) {
    return type->name()->str().cpp_str();
  } else {
    return "<" + std::to_string(reinterpret_cast<uintptr_t>(type)) + ">";
  }
}


std::string TypePrinter::Print(const MemberRequirement* type, bool paren, Status* status) const {
  std::string str = "";

  if (!options_.verbose && type->is_resolved()) {
    return Print(type->resolved()->owner(), paren, status);
  }

  if (type->is_resolved()) {
    str += Print(type->resolved()->owner(), false, status);
    str += ".";
  }
  str += type->name()->str().cpp_str();
  str += ": " + Print(type->type(), false, status);

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
  // str += std::to_string(type->depth());
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

  std::string str = "Forall (";

  bool first = true;
  for (auto var : type->vars()) {
    if (!first) {
      str += ", ";
    }
    first = false;
    str += Print(var, false, status);
  }

  str += "). ";
  str += Print(type->body(), false, status);

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
        if (ub->kind() == Type::Kind::kMemberReq) {
          auto memreq = ub->template As<MemberRequirement>();
          if (memreq->is_resolved()) {
            ub = memreq->resolved()->owner();
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
        lower_str += Print(var->lower_bound(), true, status);
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
        upper_str += Print(var->upper_bound(), true, status);
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
