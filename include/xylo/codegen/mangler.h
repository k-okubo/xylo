
#ifndef XYLO_CODEGEN_MANGLER_H_
#define XYLO_CODEGEN_MANGLER_H_

#include "xylo/syntax/context.h"

namespace xylo {


class Mangler {
 public:
  explicit Mangler(XyloContext* xylo_context) :
      xylo_context_(xylo_context) {}

  void TypeKey(HString* out, const Type* type) {
    switch (type->kind()) {
      case Type::Kind::kNominal: {
        if (type == xylo_context_->unit_type()) {
          *out += "U";
        } else if (type == xylo_context_->bool_type()) {
          *out += "B";
        } else if (type == xylo_context_->int_type()) {
          *out += "I";
        } else if (type == xylo_context_->float_type()) {
          *out += "F";
        } else if (type == xylo_context_->string_type()) {
          *out += "S";
        } else {
          auto nominal_type = type->As<NominalType>();
          auto& name = nominal_type->name()->str();
          *out += "N";
          *out += std::to_string(name.size());
          *out += name;
        }
        return;
      }

      case Type::Kind::kFunction: {
        auto func_type = type->As<FunctionType>();
        *out += "f";
        TypeKey(out, func_type->params_type());
        TypeKey(out, func_type->return_type());
        return;
      }

      case Type::Kind::kTuple: {
        auto tuple_type = type->As<TupleType>();
        *out += "t";
        *out += std::to_string(tuple_type->elements().size());
        for (auto elem : tuple_type->elements()) {
          TypeKey(out, elem);
        }
        return;
      }

      case Type::Kind::kIntersection:
        xylo_contract(type->is_top_type());
        *out += "A";
        return;

      case Type::Kind::kUnion:
        xylo_contract(type->is_bottom_type());
        *out += "N";
        return;

      case Type::Kind::kError:
      case Type::Kind::kMemberConstraint:
      case Type::Kind::kTyvar:
      case Type::Kind::kMetavar:
      case Type::Kind::kScheme:
      case Type::Kind::kApplication:
        xylo_unreachable();
    }

    xylo_unreachable();
  }


  HString TypeArgsKey(const Vector<Type*>& type_args) {
    HString key;
    key += std::to_string(type_args.size());
    for (auto t : type_args) {
      TypeKey(&key, t);
    }
    return key;
  }


 private:
  XyloContext* xylo_context_;
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_MANGLER_H_
