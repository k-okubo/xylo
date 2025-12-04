
#ifndef XYLO_SYNTAX_TYPE_H_
#define XYLO_SYNTAX_TYPE_H_

#include <memory>
#include <optional>
#include <string>
#include <utility>

#include "xylo/syntax/identifier.h"
#include "xylo/util/downcastable.h"
#include "xylo/util/map.h"
#include "xylo/util/pair_hash.h"
#include "xylo/util/set.h"
#include "xylo/util/vector.h"


namespace xylo {

class Type;
class NominalType;
class FunctionType;
class TupleType;
class IntersectionType;
class UnionType;
class TypeMetavar;
class TypeScheme;

class NominalSlot;
class MemberInfo;
class SuperInfo;

using TypePtr = std::unique_ptr<Type>;
using TypePtrVec = Vector<TypePtr>;
using FunctionTypePtr = std::unique_ptr<FunctionType>;
using IntersectionTypePtr = std::unique_ptr<IntersectionType>;
using UnionTypePtr = std::unique_ptr<UnionType>;

using MemberInfoPtr = std::unique_ptr<MemberInfo>;
using SuperInfoPtr = std::unique_ptr<SuperInfo>;
using TypePairSet = Set<std::pair<const Type*, const Type*>, PairHash>;

class Substitution;


class TypeSink {
 public:
  TypeSink() :
      type_pool_() {}
  virtual ~TypeSink() = default;

  void adopt_type(TypePtr&& type) { type_pool_.push_back(std::move(type)); }
  void adopt_types(TypeSink* other) {
    for (auto& type : other->type_pool_) {
      type_pool_.push_back(std::move(type));
    }
    other->type_pool_.clear();
  }

 private:
  Vector<TypePtr> type_pool_;
};


class Type : public TypeSink, public Downcastable {
 public:
  enum class Kind {
    kError,
    kNominal,
    kMemberReq,
    kFunction,
    kTuple,
    kIntersection,
    kUnion,
    kTyvar,
    kMetavar,
    kScheme,
  };

 protected:
  explicit Type(Kind kind) :
      kind_(kind) {}

 public:
  virtual ~Type() = default;

  Type(const Type&) = delete;
  Type& operator=(const Type&) = delete;

  Kind kind() const { return kind_; }

  virtual bool equals(const Type* other) const = 0;
  virtual bool is_atomic_type() const { return false; }
  virtual bool is_function_type() const { return false; }
  virtual bool is_top_type() const { return false; }
  virtual bool is_bottom_type() const { return false; }
  virtual bool is_var_type() const { return false; }

  // check this <: dst
  bool IsSubtypeOf(const Type* dst) const {
    TypePairSet visited;
    return IsSubtypeOf(dst, &visited);
  }
  bool IsSubtypeOf(const Type* dst, TypePairSet* visited) const;

  // can mark this <: dst ?
  bool CanConstrainSubtypeOf(const Type* dst) const {
    TypePairSet visited;
    return CanConstrainSubtypeOf(dst, &visited);
  }
  bool CanConstrainSubtypeOf(const Type* dst, TypePairSet* visited) const;

  // mark this <: dst
  bool ConstrainSubtypeOf(Type* dst) {
    TypePairSet visited;
    return ConstrainSubtypeOf(dst, &visited);
  }
  bool ConstrainSubtypeOf(Type* dst, TypePairSet* visited);

  // mark other <: this <: other, this <: other <: this
  bool ConstrainSameAs(Type* other);

  virtual Type* CloseOverMetavars(int depth, TypeSink* out_allocated) = 0;
  void PruneInnerScopeVars(int depth);
  Type* Generalize(int depth, TypeSink* out_allocated);
  virtual Type* Instantiate(TypeSink* out_allocated, Vector<TypeMetavar*>* out_instantiated_vars) const;

  virtual Type* Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) = 0;
  bool IsGroundType() const;

 private:
  Kind kind_;
};


class ErrorType : public Type {
 public:
  static auto instance() {
    static ErrorType instance;
    return &instance;
  }

 protected:
  ErrorType() :
      Type(Kind::kError) {}

 public:
  bool equals(const Type* other) const override;
  Type* CloseOverMetavars(int depth, TypeSink* out_allocated) override;
  Type* Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) override;
};


class NominalType : public Type {
 public:
  enum class Category {
    kInterface,
    kClass,
    kPrimitive,
    kConcept,
  };

  explicit NominalType(Category category, Identifier* name) :
      Type(Kind::kNominal),
      category_(category),
      name_(name),
      fields_(),
      methods_(),
      embeddings_(),
      supers_(),
      member_map_(),
      super_infos_() {}

  Category category() const { return category_; }
  Identifier* name() const { return name_; }

  const Vector<MemberInfo*>& fields() const { return fields_; }
  const Vector<MemberInfo*>& methods() const { return methods_; }
  const Vector<MemberInfo*>& embeddings() const { return embeddings_; }
  const Vector<NominalType*>& supers() const { return supers_; }

  bool equals(const Type* other) const override;
  bool is_atomic_type() const override { return true; }

  MemberInfo* GetMember(Identifier* member_name, bool include_super = true) const;
  MemberInfo* GetDeclaredMember(Identifier* member_name) const;
  bool GetMemberPath(Identifier* member_name, Vector<NominalSlot*>* out_path) const;
  bool GetSuperPath(NominalType* super, Vector<NominalSlot*>* out_path) const;
  void FindEmbededMembers(Identifier* member_name, Vector<MemberInfo*>* out_members) const;
  void FindSuperMembers(Identifier* member_name, Vector<MemberInfo*>* out_members) const;
  void FindSuperMethods(Vector<MemberInfo*>* out_members) const;

  MemberInfo* AddField(Identifier* field_name, Type* field_type);
  MemberInfo* AddMethod(Identifier* method_name, Type* method_type);
  MemberInfo* AddEmbedding(Identifier* field_name, NominalType* clazz);
  bool AddSuper(NominalType* clazz);

  Type* CloseOverMetavars(int depth, TypeSink* out_allocated) override;
  Type* Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) override;

 private:
  Category category_;
  Identifier* name_;
  Vector<MemberInfo*> fields_;
  Vector<MemberInfo*> methods_;
  Vector<MemberInfo*> embeddings_;
  Vector<NominalType*> supers_;

  Map<Identifier*, MemberInfoPtr> member_map_;
  Vector<SuperInfoPtr> super_infos_;
};


class NominalSlot {
 public:
  enum class Kind {
    kField,
    kMethod,
    kEmbedding,
    kSuper,
  };

  NominalSlot(NominalType* owner, Kind kind, int index) :
      owner_(owner),
      kind_(kind),
      index_(index) {}

  virtual ~NominalSlot() = default;

  NominalType* owner() const { return owner_; }
  Kind kind() const { return kind_; }
  int index() const { return index_; }

 private:
  NominalType* owner_;
  Kind kind_;
  int index_;
};


class MemberInfo : public NominalSlot {
 public:
  MemberInfo(NominalType* owner, Kind kind, int index, Identifier* name, Type* type) :
      NominalSlot(owner, kind, index),
      name_(name),
      type_(type),
      type_resolver_() {}

  Identifier* name() const { return name_; }

  Type* type() const { return type_; }
  void set_type(Type* type) { type_ = type; }

  void set_type_resolver(std::function<void()> resolver) { type_resolver_ = std::move(resolver); }
  void resolve_type() {
    if (type_ == nullptr && type_resolver_) {
      type_resolver_();
      type_resolver_ = nullptr;
    }
  }

 private:
  Identifier* name_;
  Type* type_;
  std::function<void()> type_resolver_;
};


class SuperInfo : public NominalSlot {
 public:
  SuperInfo(NominalType* owner, int index, NominalType* type) :
      NominalSlot(owner, Kind::kSuper, index),
      type_(type) {}

  NominalType* type() const { return type_; }

 private:
  NominalType* type_;
};


class MemberRequirement : public Type {
 public:
  MemberRequirement(Identifier* name, Type* type) :
      Type(Kind::kMemberReq),
      name_(name),
      type_(type),
      mutable_(false),
      resolved_(nullptr),
      instantiated_vars_(),
      origin_owner_(nullptr),
      on_error_(nullptr) {}

  Identifier* name() const { return name_; }

  Type* type() const { return type_; }
  void set_type(Type* type) { type_ = type; }

  bool is_mutable() const { return mutable_; }
  bool SetMutable(bool m);

  bool is_resolved() const { return resolved_ != nullptr; }
  MemberInfo* resolved() const { return resolved_; }
  void set_resolved(MemberInfo* resolved) { resolved_ = resolved; }

  const Vector<TypeMetavar*>& instantiated_vars() const { return instantiated_vars_; }
  void set_instantiated_vars(Vector<TypeMetavar*>&& vars) { instantiated_vars_ = std::move(vars); }

  Type* origin_owner() const { return origin_owner_; }
  void set_origin_owner(Type* owner) { origin_owner_ = owner; }

  std::function<void(const NominalType*)> on_error() const { return on_error_; }
  void set_on_error(std::function<void(const NominalType*)> on_error) { on_error_ = on_error; }

  bool equals(const Type* other) const override;
  bool is_atomic_type() const override { return true; }

  bool CanAccept(const NominalType* nominal, TypePairSet* visited) const;
  bool Accept(NominalType* nominal, TypePairSet* visited);

  void CallbackOnError(const NominalType* nominal) const {
    if (on_error_) {
      on_error_(nominal);
    }
  }

  Type* CloseOverMetavars(int depth, TypeSink* out_allocated) override;
  Type* Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) override;

 private:
  Identifier* name_;
  Type* type_;
  bool mutable_;
  MemberInfo* resolved_;
  Vector<TypeMetavar*> instantiated_vars_;
  Type* origin_owner_;
  std::function<void(const NominalType*)> on_error_;
};


class FunctionType : public Type {
 public:
  FunctionType(bool closure, TupleType* params_type, Type* return_type) :
      Type(Kind::kFunction),
      closure_(closure),
      params_type_(params_type),
      return_type_(return_type) {}

  TupleType* params_type() const { return params_type_; }
  Type* return_type() const { return return_type_; }

  bool is_closure() const { return closure_; }
  void set_closure(bool closure) { closure_ = closure; }

  bool equals(const Type* other) const override;
  bool is_function_type() const override { return true; }

  Type* CloseOverMetavars(int depth, TypeSink* out_allocated) override;
  Type* Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) override;

 private:
  bool closure_;
  TupleType* params_type_;
  Type* return_type_;
};


class TupleType : public Type {
 public:
  TupleType() :
      Type(Kind::kTuple),
      elements_() {}

  TupleType(std::initializer_list<Type*> init) :
      Type(Kind::kTuple),
      elements_(init) {}

  const Vector<Type*>& elements() const { return elements_; }
  void add_element(Type* type) { elements_.push_back(type); }

  bool empty() const { return elements_.empty(); }
  bool equals(const Type* other) const override;

  Type* CloseOverMetavars(int depth, TypeSink* out_allocated) override;
  Type* Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) override;

 private:
  Vector<Type*> elements_;
};


class IntersectionType : public Type {
 public:
  IntersectionType() :
      Type(Kind::kIntersection),
      elements_(),
      shrinked_(true) {}

  IntersectionType(std::initializer_list<Type*> init) :
      Type(Kind::kIntersection),
      elements_(init),
      shrinked_(false) {}

  const Vector<Type*>& elements() const { return elements_; }
  void add_element(Type* type) {
    elements_.push_back(type);
    shrinked_ = false;
  }

  bool empty() const { return elements_.empty(); }
  bool equals(const Type* other) const override;
  bool is_top_type() const override { return empty(); }

  void ShrinkToLower();

  Type* CloseOverMetavars(int depth, TypeSink* out_allocated) override;
  Type* Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) override;

 private:
  Vector<Type*> elements_;
  bool shrinked_;
};


class UnionType : public Type {
 public:
  UnionType() :
      Type(Kind::kUnion),
      elements_(),
      shrinked_(true) {}

  UnionType(std::initializer_list<Type*> init) :
      Type(Kind::kUnion),
      elements_(init),
      shrinked_(false) {}

  const Vector<Type*>& elements() const { return elements_; }
  void add_element(Type* type) {
    elements_.push_back(type);
    shrinked_ = false;
  }

  bool empty() const { return elements_.empty(); }
  bool equals(const Type* other) const override;
  bool is_bottom_type() const override { return empty(); }

  void ShrinkToUpper();

  Type* CloseOverMetavars(int depth, TypeSink* out_allocated) override;
  Type* Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) override;

 private:
  Vector<Type*> elements_;
  bool shrinked_;
};


class VariableBase {
 public:
  VariableBase(Type* owner, std::function<Type*()> func_rettype) :
      owner_(owner),
      upper_bound_(new IntersectionType()),
      lower_bound_(new UnionType()),
      func_shape_(nullptr),
      lower_based_closure_(false),
      func_rettype_(func_rettype) {}

  ~VariableBase() = default;

  const IntersectionType* upper_bound() const { return upper_bound_.get(); }
  const UnionType* lower_bound() const { return lower_bound_.get(); }
  FunctionType* func_shape() const { return func_shape_; }
  bool is_lower_based_closure() const { return lower_based_closure_; }

  void set_upper_bound(IntersectionTypePtr&& upper_bound) { upper_bound_ = std::move(upper_bound); }
  void set_lower_bound(UnionTypePtr&& lower_bound) { lower_bound_ = std::move(lower_bound); }
  void set_func_shape(FunctionType* func_shape, bool lower_based_closure) {
    func_shape_ = func_shape;
    lower_based_closure_ = lower_based_closure;
  }

  bool CanConstrainUpperBound(const Type* new_ub, TypePairSet* visited) const;
  bool CanConstrainLowerBound(const Type* new_lb, TypePairSet* visited) const;

  bool ConstrainUpperBound(Type* new_ub, TypePairSet* visited);
  bool ConstrainLowerBound(Type* new_lb, TypePairSet* visited);
  bool ConstrainBothBounds(VariableBase* dst, TypePairSet* visited);

 protected:
  bool ConstrainUpperBoundForFunc(Type* new_ub, TypePairSet* visited);
  bool ConstrainUpperBoundForAtom(Type* new_ub, TypePairSet* visited);

  bool ConstrainLowerBoundForFunc(Type* new_lb, TypePairSet* visited);
  bool ConstrainLowerBoundForAtom(Type* new_lb, TypePairSet* visited);

  void Functionize(const FunctionType* base_shape, bool lower_based);


 private:
  Type* owner_;

  IntersectionTypePtr upper_bound_;
  UnionTypePtr lower_bound_;

  FunctionType* func_shape_;
  bool lower_based_closure_;  // whether the lower side set the closure flag of func_shape
  std::function<Type*()> func_rettype_;
};


class TypeVariable : public Type {
 protected:
  static constexpr auto func_rettype(int depth) {
    return [depth]() {
      return new TypeVariable(depth);
    };
  }

 public:
  explicit TypeVariable(int depth) :
      Type(Kind::kTyvar),
      varbase_(this, func_rettype(depth)),
      depth_(depth) {}

  int depth() const { return depth_; }
  void set_depth(int depth) { depth_ = depth; }

  auto upper_bound() const { return varbase_.upper_bound(); }
  auto lower_bound() const { return varbase_.lower_bound(); }
  auto func_shape() const { return varbase_.func_shape(); }
  auto is_lower_based_closure() const { return varbase_.is_lower_based_closure(); }

  bool equals(const Type* other) const override;
  bool is_atomic_type() const override { return true; }
  bool is_function_type() const override { return varbase_.func_shape() != nullptr; }
  bool is_var_type() const override { return true; }

  Type* CloseOverMetavars(int depth, TypeSink* out_allocated) override;
  void PruneInnerScopeVars();
  Type* Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) override;

 protected:
  bool CanConstrainUpperBound(const Type* new_ub, TypePairSet* visited) const {
    return varbase_.CanConstrainUpperBound(new_ub, visited);
  }
  bool CanConstrainLowerBound(const Type* new_lb, TypePairSet* visited) const {
    return varbase_.CanConstrainLowerBound(new_lb, visited);
  }

  bool ConstrainUpperBound(Type* new_ub, TypePairSet* visited) { return varbase_.ConstrainUpperBound(new_ub, visited); }
  bool ConstrainLowerBound(Type* new_lb, TypePairSet* visited) { return varbase_.ConstrainLowerBound(new_lb, visited); }
  bool ConstrainBothBounds(TypeVariable* dst, TypePairSet* visited) {
    return varbase_.ConstrainBothBounds(&dst->varbase_, visited);
  }

 private:
  VariableBase varbase_;
  int depth_;

  friend class Type;
};


class TypeMetavar : public Type {
 protected:
  static constexpr auto func_rettype() {
    return []() {
      return new TypeMetavar();
    };
  }

 public:
  TypeMetavar() :
      Type(Kind::kMetavar),
      varbase_(this, func_rettype()) {}

  auto upper_bound() const { return varbase_.upper_bound(); }
  auto lower_bound() const { return varbase_.lower_bound(); }
  auto func_shape() const { return varbase_.func_shape(); }
  auto is_lower_based_closure() const { return varbase_.is_lower_based_closure(); }

  bool equals(const Type* other) const override;
  bool is_atomic_type() const override { return true; }
  bool is_function_type() const override { return varbase_.func_shape() != nullptr; }
  bool is_var_type() const override { return true; }

  Type* CloseOverMetavars(int depth, TypeSink* out_allocated) override;
  Type* Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) override;

 protected:
  void set_upper_bound(IntersectionTypePtr&& upper_bound) { varbase_.set_upper_bound(std::move(upper_bound)); }
  void set_lower_bound(UnionTypePtr&& lower_bound) { varbase_.set_lower_bound(std::move(lower_bound)); }
  void set_func_shape(FunctionType* func_shape, bool lower_based_closure) {
    varbase_.set_func_shape(func_shape, lower_based_closure);
  }

  bool CanConstrainUpperBound(const Type* new_ub, TypePairSet* visited) const {
    return varbase_.CanConstrainUpperBound(new_ub, visited);
  }
  bool CanConstrainLowerBound(const Type* new_lb, TypePairSet* visited) const {
    return varbase_.CanConstrainLowerBound(new_lb, visited);
  }

  bool ConstrainUpperBound(Type* new_ub, TypePairSet* visited) { return varbase_.ConstrainUpperBound(new_ub, visited); }
  bool ConstrainLowerBound(Type* new_lb, TypePairSet* visited) { return varbase_.ConstrainLowerBound(new_lb, visited); }
  bool ConstrainBothBounds(TypeMetavar* dst, TypePairSet* visited) {
    return varbase_.ConstrainBothBounds(&dst->varbase_, visited);
  }

 private:
  VariableBase varbase_;

  friend class Type;
  friend class TypeScheme;
};


class TypeScheme : public Type {
 public:
  explicit TypeScheme(Vector<TypeVariable*>&& vars, Type* body) :
      Type(Kind::kScheme),
      vars_(std::move(vars)),
      body_(body) {}

  const Vector<TypeVariable*>& vars() const { return vars_; }
  Type* body() const { return body_; }

  bool equals(const Type* other) const override;

  Type* CloseOverMetavars(int depth, TypeSink* out_allocated) override;
  Type* Instantiate(TypeSink* out_allocated, Vector<TypeMetavar*>* out_instantiated_vars) const override;
  Type* Zonk(const Substitution* env, bool strict, TypeSink* out_allocated) override;

 private:
  Vector<TypeVariable*> vars_;
  Type* body_;
};


struct TypePrinterOptions {
  bool verbose = false;
};

class TypePrinter {
 public:
  using NameMap = Map<const Type*, std::string>;
  using TypeVarVec = Vector<const Type*>;

  explicit TypePrinter(TypePrinterOptions options = {}) :
      options_(options) {}

  ~TypePrinter() = default;

  std::string operator()(const Type* type) const {
    NameMap name_map;
    return this->operator()(type, &name_map);
  }

  std::string operator()(const Type* type, NameMap* name_map) const {
    Status status(name_map);
    auto str = Print(type, false, &status);
    str += PrintConstraints(&status);
    return str;
  }

 protected:
  struct Status {
    NameMap* name_map;
    TypeVarVec appeared;
    Map<const Type*, const Type*> alias_map;

    explicit Status(NameMap* name_map) :
        name_map(name_map),
        appeared(),
        alias_map() {}
  };

  std::string Print(const Type* type, bool paren, Status* status) const;
  std::string Print(const NominalType* type, bool paren, Status* status) const;
  std::string Print(const MemberRequirement* type, bool paren, Status* status) const;
  std::string Print(const FunctionType* type, bool paren, Status* status) const;
  std::string Print(const TupleType* type, bool paren, Status* status) const;
  std::string Print(const IntersectionType* type, bool paren, Status* status) const;
  std::string Print(const UnionType* type, bool paren, Status* status) const;
  std::string Print(const TypeVariable* type, bool paren, Status* status) const;
  std::string Print(const TypeMetavar* type, bool paren, Status* status) const;
  std::string Print(const TypeScheme* type, bool paren, Status* status) const;

  std::string PrintConstraints(Status* status) const;

 private:
  TypePrinterOptions options_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_TYPE_H_
