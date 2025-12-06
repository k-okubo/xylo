
#include "xylo/syntax/substitution.h"

#include "xylo/syntax/type.h"

namespace xylo {


Type* Substitution::Apply(Type* type, TypeArena* arena) const {
  switch (type->kind()) {
    case Type::Kind::kError:
      return type;

    case Type::Kind::kNominal:
      return type;

    case Type::Kind::kMemberConstraint: {
      auto memcon = type->As<MemberConstraint>();
      auto sbsted_type = Apply(memcon->type(), arena);
      if (sbsted_type == memcon->type()) {
        return memcon;
      } else {
        auto new_memcon = new MemberConstraint(memcon->name(), sbsted_type);
        new_memcon->SetMutable(memcon->is_mutable());
        new_memcon->set_resolved(memcon->resolved());
        new_memcon->set_origin_owner(memcon->origin_owner());
        new_memcon->set_on_error(memcon->on_error());
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

    case Type::Kind::kTuple: {
      auto tuple = type->As<TupleType>();
      auto new_tuple = new TupleType();
      bool changed = false;
      for (auto elem : tuple->elements()) {
        auto sbsted_elem = Apply(elem, arena);
        new_tuple->add_element(sbsted_elem);
        changed |= (sbsted_elem != elem);
      }

      if (changed) {
        arena->adopt_type(TypePtr(new_tuple));
        return new_tuple;
      } else {
        delete new_tuple;
        return tuple;
      }
    }

    case Type::Kind::kIntersection: {
      auto inter = type->As<IntersectionType>();
      auto new_inter = new IntersectionType();
      bool changed = false;
      for (auto elem : inter->elements()) {
        auto sbsted_elem = Apply(elem, arena);
        new_inter->add_element(sbsted_elem);
        changed |= (sbsted_elem != elem);
      }

      if (changed) {
        arena->adopt_type(TypePtr(new_inter));
        return new_inter;
      } else {
        delete new_inter;
        return inter;
      }
    }

    case Type::Kind::kUnion: {
      auto uni = type->As<UnionType>();
      auto new_uni = new UnionType();
      bool changed = false;
      for (auto elem : uni->elements()) {
        auto sbsted_elem = Apply(elem, arena);
        new_uni->add_element(sbsted_elem);
        changed |= (sbsted_elem != elem);
      }

      if (changed) {
        arena->adopt_type(TypePtr(new_uni));
        return new_uni;
      } else {
        delete new_uni;
        return uni;
      }
    }

    case Type::Kind::kTyvar: {
      auto var = type->As<TypeVariable>();
      auto it = subst_.find(var);
      if ((it != subst_.end())) {
        return it->second;
      } else if (parent_ != nullptr) {
        return parent_->Apply(var, arena);
      } else {
        return var;
      }
    }

    case Type::Kind::kMetavar:
      return type;

    case Type::Kind::kScheme:
      return type;
  }

  xylo_unreachable();
}


}  // namespace xylo
