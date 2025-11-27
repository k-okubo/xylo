
#include "xylo/syntax/substitution.h"

#include "xylo/syntax/type.h"

namespace xylo {


Type* Substitution::Apply(Type* type, TypeSink* out_allocated) const {
  switch (type->kind()) {
    case Type::Kind::kError:
      return type;

    case Type::Kind::kNominal:
      return type;

    case Type::Kind::kMemberReq: {
      auto memreq = type->As<MemberRequirement>();
      auto sbsted_type = Apply(memreq->type(), out_allocated);
      if (sbsted_type == memreq->type()) {
        return memreq;
      } else {
        auto new_memreq = new MemberRequirement(memreq->name(), sbsted_type);
        new_memreq->SetMutable(memreq->is_mutable());
        new_memreq->set_resolved(memreq->resolved());
        new_memreq->set_on_error(memreq->on_error());
        out_allocated->adopt_type(TypePtr(new_memreq));
        return new_memreq;
      }
    }

    case Type::Kind::kFunction: {
      auto func = type->As<FunctionType>();
      auto sbsted_params = Apply(func->params_type(), out_allocated);
      auto sbsted_return = Apply(func->return_type(), out_allocated);

      if (sbsted_params == func->params_type() && sbsted_return == func->return_type()) {
        return func;
      } else {
        auto new_func = new FunctionType(func->is_closure(), sbsted_params->As<TupleType>(), sbsted_return);
        out_allocated->adopt_type(TypePtr(new_func));
        return new_func;
      }
    }

    case Type::Kind::kTuple: {
      auto tuple = type->As<TupleType>();
      auto new_tuple = new TupleType();
      bool changed = false;
      for (auto elem : tuple->elements()) {
        auto sbsted_elem = Apply(elem, out_allocated);
        new_tuple->add_element(sbsted_elem);
        changed |= (sbsted_elem != elem);
      }

      if (changed) {
        out_allocated->adopt_type(TypePtr(new_tuple));
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
        auto sbsted_elem = Apply(elem, out_allocated);
        new_inter->add_element(sbsted_elem);
        changed |= (sbsted_elem != elem);
      }

      if (changed) {
        out_allocated->adopt_type(TypePtr(new_inter));
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
        auto sbsted_elem = Apply(elem, out_allocated);
        new_uni->add_element(sbsted_elem);
        changed |= (sbsted_elem != elem);
      }

      if (changed) {
        out_allocated->adopt_type(TypePtr(new_uni));
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
        return parent_->Apply(var, out_allocated);
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
