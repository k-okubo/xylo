
#ifndef XYLO_SYNTAX_TYPE_H_
#define XYLO_SYNTAX_TYPE_H_

#include <memory>
#include <optional>
#include <string>
#include <utility>

#include "xylo/syntax/identifier.h"
#include "xylo/syntax/scope.h"
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
class Substitution;

class NominalSlot;
class MemberInfo;
class SuperInfo;

using TypePtr = std::unique_ptr<Type>;
using TypePtrVec = Vector<TypePtr>;
using FunctionTypePtr = std::unique_ptr<FunctionType>;
using IntersectionTypePtr = std::unique_ptr<IntersectionType>;
using UnionTypePtr = std::unique_ptr<UnionType>;
using SubstitutionPtr = std::unique_ptr<Substitution>;

using MemberInfoPtr = std::unique_ptr<MemberInfo>;
using SuperInfoPtr = std::unique_ptr<SuperInfo>;
using TypePairSet = Set<std::pair<const Type*, const Type*>, PairHash>;


class TypeArena {
 public:
  TypeArena() :
      pool_() {}
  virtual ~TypeArena() = default;

  void adopt_type(TypePtr&& type) { pool_.push_back(std::move(type)); }
  void merge(TypeArena* other) {
    for (auto& type : other->pool_) {
      pool_.push_back(std::move(type));
    }
    other->pool_.clear();
  }

 private:
  Vector<TypePtr> pool_;
};


struct InstantiatedInfo {
  Vector<Type*> vars;
};
using InstantiatedInfoPtr = std::unique_ptr<InstantiatedInfo>;


class Type : public Downcastable {
 public:
  enum class Kind {
    kError,
    kNominal,
    kMemberConstraint,
    kFunction,
    kTuple,
    kIntersection,
    kUnion,
    kTyvar,
    kMetavar,
    kScheme,
    kApplication,
  };

 protected:
  explicit Type(Kind kind) :
      kind_(kind),
      instantiated_info_(nullptr),
      instantiated_ref_(nullptr),
      arena_() {}

 public:
  virtual ~Type() = default;

  Type(const Type&) = delete;
  Type& operator=(const Type&) = delete;

  Kind kind() const { return kind_; }
  TypeArena* arena() { return &arena_; }

  virtual bool equals(const Type* other) const = 0;
  virtual bool is_atomic_type() const { return false; }
  virtual bool is_function_type() const { return false; }
  virtual bool is_top_type() const { return false; }
  virtual bool is_bottom_type() const { return false; }
  virtual bool is_var_type() const { return false; }
  bool is_closed_type() const;

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

  Type* CloseOverMetavars(Scope* scope, TypeArena* arena) {
    Map<TypeMetavar*, Type*> map;
    return CloseOverMetavars(scope, arena, &map);
  }
  virtual Type* CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) = 0;

  Type* Generalize(Scope* scope, TypeArena* arena);
  virtual Type* Instantiate(TypeArena* arena, const Vector<Type*>& args = {}, Substitution* subst = nullptr) const;

  const InstantiatedInfo* instantiated_info() const { return instantiated_ref_; }
  void set_instantiated_info(InstantiatedInfoPtr&& info) {
    instantiated_ref_ = info.get();
    instantiated_info_ = std::move(info);
  }
  void set_instantiated_info(const InstantiatedInfo* info) {
    instantiated_ref_ = info;
    instantiated_info_.reset();
  }

  virtual Type* Zonk(const Substitution* subst, bool strict, TypeArena* arena) = 0;

 private:
  Kind kind_;
  InstantiatedInfoPtr instantiated_info_;
  const InstantiatedInfo* instantiated_ref_;
  TypeArena arena_;
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
  Type* CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) override;
  Type* Zonk(const Substitution* subst, bool strict, TypeArena* arena) override;
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
      scope_(nullptr),
      generic_(false),
      origin_(nullptr),
      substitution_(nullptr),
      member_copied_(false),
      locked_(false),
      supers_(),
      embeddeds_(),
      fields_(),
      methods_(),
      super_infos_(),
      member_map_() {}

  Category category() const { return category_; }
  Identifier* name() const { return name_; }

  Scope* scope() const { return scope_; }
  void set_scope(Scope* scope) { scope_ = scope; }

  bool is_generic() const { return generic_; }
  void set_generic(bool generic) { generic_ = generic; }

  NominalType* origin() const { return origin_; }
  void set_origin(NominalType* origin) { origin_ = origin; }

  Substitution* substitution() const { return substitution_.get(); }
  void set_substitution(SubstitutionPtr&& subst) { substitution_ = std::move(subst); }

  bool is_locked() const { return locked_; }
  void lock() { locked_ = true; }

  bool equals(const Type* other) const override;
  bool is_atomic_type() const override { return true; }

  const NominalType* canonical() const { return origin_ != nullptr ? origin_->canonical() : this; }
  NominalType* canonical() { return origin_ != nullptr ? origin_->canonical() : this; }

  const Vector<NominalType*>& supers() const {
    EnsureMembers();
    return supers_;
  }

  const Vector<MemberInfo*>& embeddeds() const {
    EnsureMembers();
    return embeddeds_;
  }

  const Vector<MemberInfo*>& fields() const {
    EnsureMembers();
    return fields_;
  }

  const Vector<MemberInfo*>& methods() const {
    EnsureMembers();
    return methods_;
  }

  MemberInfo* GetMember(Identifier* member_name, bool include_super = true) const;
  MemberInfo* GetDeclaredMember(Identifier* member_name) const;
  bool GetMemberPath(Identifier* member_name, Vector<NominalSlot*>* out_path) const;
  bool GetSuperPath(NominalType* super, Vector<NominalSlot*>* out_path) const;

  void FindEmbededMembers(Identifier* member_name, Vector<MemberInfo*>* out_members) const;
  void FindSuperMembers(Identifier* member_name, Vector<MemberInfo*>* out_members) const;
  void FindSuperMethods(Vector<MemberInfo*>* out_members) const;

  bool HasSuper(const NominalType* target) const;
  bool HasEmbedded(const NominalType* target) const;

  bool AddSuper(NominalType* clazz);
  MemberInfo* AddEmbedded(Identifier* field_name, NominalType* clazz);
  MemberInfo* AddField(Identifier* field_name, Type* field_type);
  MemberInfo* AddMethod(Identifier* method_name, Type* method_type);

  Type* CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) override;
  Type* Zonk(const Substitution* subst, bool strict, TypeArena* arena) override;

 protected:
  void EnsureMembers() const {
    if (origin_ != nullptr && !member_copied_) {
      const_cast<NominalType*>(this)->CopyMembersFromOrigin();
    }
  }
  void CopyMembersFromOrigin();

 private:
  Category category_;
  Identifier* name_;
  Scope* scope_;

  bool generic_;
  NominalType* origin_;
  SubstitutionPtr substitution_;
  bool member_copied_;

  bool locked_;
  Vector<NominalType*> supers_;
  Vector<MemberInfo*> embeddeds_;
  Vector<MemberInfo*> fields_;
  Vector<MemberInfo*> methods_;

  Vector<SuperInfoPtr> super_infos_;
  Map<Identifier*, MemberInfoPtr> member_map_;
};


class NominalSlot : public Downcastable {
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


class MemberConstraint : public Type {
 public:
  using ErrorCallback = std::function<void(const NominalType*, const NominalType*)>;

  MemberConstraint(Identifier* member_name, Type* expected_type) :
      Type(Kind::kMemberConstraint),
      member_name_(member_name),
      expected_type_(expected_type),
      mutable_(false),
      on_error_(nullptr),
      resolved_in_(nullptr),
      resolved_member_(nullptr) {}

  Identifier* member_name() const { return member_name_; }

  Type* expected_type() const { return expected_type_; }
  void set_expected_type(Type* expected_type) { expected_type_ = expected_type; }

  bool is_mutable() const { return mutable_; }
  bool SetMutable(bool m);

  ErrorCallback on_error() const { return on_error_; }
  void set_on_error(ErrorCallback on_error) { on_error_ = on_error; }

  bool is_resolved() const { return resolved_in_ != nullptr; }

  NominalType* resolved_in() const { return resolved_in_; }
  void set_resolved_in(NominalType* resolved_in) { resolved_in_ = resolved_in; }

  MemberInfo* resolved_member() const { return resolved_member_; }
  void set_resolved_member(MemberInfo* resolved_member) { resolved_member_ = resolved_member; }

  bool equals(const Type* other) const override;
  bool is_atomic_type() const override { return true; }

  bool CanResolve(const NominalType* nominal, TypePairSet* visited) const;
  bool Resolve(NominalType* nominal, TypePairSet* visited);

 protected:
  std::pair<NominalType*, MemberInfo*> FindConstraintMember(NominalType* nominal) const;

  void CallbackOnError(const NominalType* nominal, const NominalType* other = nullptr) const {
    if (on_error_) {
      on_error_(nominal, other);
    }
  }

  Type* CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) override;
  Type* Zonk(const Substitution* subst, bool strict, TypeArena* arena) override;

 private:
  Identifier* member_name_;
  Type* expected_type_;
  bool mutable_;
  ErrorCallback on_error_;

  NominalType* resolved_in_;
  MemberInfo* resolved_member_;
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

  Type* CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) override;
  Type* Zonk(const Substitution* subst, bool strict, TypeArena* arena) override;

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

  Type* CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) override;
  Type* Zonk(const Substitution* subst, bool strict, TypeArena* arena) override;

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

  Type* CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) override;
  Type* Zonk(const Substitution* subst, bool strict, TypeArena* arena) override;

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

  Type* CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) override;
  Type* Zonk(const Substitution* subst, bool strict, TypeArena* arena) override;

 private:
  Vector<Type*> elements_;
  bool shrinked_;
};


class VariableBase : public Type {
 public:
  VariableBase(Kind kind, std::function<Type*()> create_var) :
      Type(kind),
      locked_(false),
      upper_bound_(new IntersectionType()),
      lower_bound_(new UnionType()),
      func_shape_(nullptr),
      upper_func_shape_(nullptr),
      lower_func_shape_(nullptr),
      create_var_(create_var) {}

  bool is_locked() const { return locked_; }
  void lock() { locked_ = true; }

  IntersectionType* upper_bound() const { return upper_bound_.get(); }
  UnionType* lower_bound() const { return lower_bound_.get(); }
  FunctionType* func_shape() const { return lower_func_shape() ? lower_func_shape() : upper_func_shape(); }

 protected:
  void set_upper_bound(IntersectionTypePtr&& upper_bound) { upper_bound_ = std::move(upper_bound); }
  void set_lower_bound(UnionTypePtr&& lower_bound) { lower_bound_ = std::move(lower_bound); }

  FunctionType* upper_func_shape() const { return upper_func_shape_.get(); }
  FunctionType* lower_func_shape() const { return lower_func_shape_.get(); }
  void set_func_shape(FunctionType* func_shape) { func_shape_ = func_shape; }

  void set_upper_func_shape(const FunctionType* upper_func_shape) {
    if (upper_func_shape) {
      upper_func_shape_ = std::make_unique<FunctionType>(upper_func_shape->is_closure(), func_shape_->params_type(),
                                                         func_shape_->return_type());
    } else {
      upper_func_shape_ = nullptr;
    }
  }

  void set_lower_func_shape(const FunctionType* lower_func_shape) {
    if (lower_func_shape) {
      lower_func_shape_ = std::make_unique<FunctionType>(lower_func_shape->is_closure(), func_shape_->params_type(),
                                                         func_shape_->return_type());
    } else {
      lower_func_shape_ = nullptr;
    }
  }

  bool CanConstrainUpperBound(const Type* new_ub, TypePairSet* visited) const;
  bool CanConstrainLowerBound(const Type* new_lb, TypePairSet* visited) const;

  bool ConstrainUpperBound(Type* new_ub, TypePairSet* visited);
  bool ConstrainLowerBound(Type* new_lb, TypePairSet* visited);
  bool ConstrainBothBounds(VariableBase* dst, TypePairSet* visited);

  bool AddToUpperBound(Type* new_ub, TypePairSet* visited);
  bool AddToLowerBound(Type* new_lb, TypePairSet* visited);

  void Functionize(const FunctionType* base_shape);
  bool GenUpperBoundFuncShape(FunctionType* new_ub, TypePairSet* visited);
  bool GenLowerBoundFuncShape(FunctionType* new_lb, TypePairSet* visited);

 private:
  bool locked_;

  IntersectionTypePtr upper_bound_;
  UnionTypePtr lower_bound_;

  FunctionType* func_shape_;
  FunctionTypePtr upper_func_shape_;
  FunctionTypePtr lower_func_shape_;
  std::function<Type*()> create_var_;

  friend class Type;
  friend class TypeScheme;
};


class TypeVariable : public VariableBase {
 protected:
  static constexpr auto create_var(Scope* scope) {
    return [scope]() {
      return new TypeVariable(scope);
    };
  }

 public:
  explicit TypeVariable(Scope* scope) :
      VariableBase(Kind::kTyvar, create_var(scope)),
      scope_(scope),
      name_(nullptr) {}

  Scope* scope() const { return scope_; }
  void set_scope(Scope* scope) { scope_ = scope; }

  Identifier* name() const { return name_; }
  void set_name(Identifier* name) { name_ = name; }

  bool equals(const Type* other) const override;
  bool is_atomic_type() const override { return true; }
  bool is_function_type() const override { return func_shape() != nullptr; }
  bool is_var_type() const override { return true; }

  Type* CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) override;
  Type* Zonk(const Substitution* subst, bool strict, TypeArena* arena) override;

 private:
  Scope* scope_;
  Identifier* name_;
};


class TypeMetavar : public VariableBase {
 protected:
  static constexpr auto create_var() {
    return []() {
      return new TypeMetavar();
    };
  }

 public:
  TypeMetavar() :
      VariableBase(Kind::kMetavar, create_var()) {}
  bool equals(const Type* other) const override;
  bool is_atomic_type() const override { return true; }
  bool is_function_type() const override { return func_shape() != nullptr; }
  bool is_var_type() const override { return true; }

  Type* CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) override;
  Type* Zonk(const Substitution* subst, bool strict, TypeArena* arena) override;
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

  Type* CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) override;
  Type* Instantiate(TypeArena* arena, const Vector<Type*>& args = {}, Substitution* subst = nullptr) const override;
  Type* Zonk(const Substitution* subst, bool strict, TypeArena* arena) override;

 private:
  Vector<TypeVariable*> vars_;
  Type* body_;
};


class TypeApplication : public Type {
 public:
  TypeApplication(Type* target, Substitution* substitution) :
      Type(Kind::kApplication),
      target_(target),
      substitution_(substitution) {}

  Type* target() const { return target_; }
  Substitution* substitution() const { return substitution_; }

  bool equals(const Type* other) const override;

  Type* CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map) override;
  Type* Instantiate(TypeArena* arena, const Vector<Type*>& args = {}, Substitution* subst = nullptr) const override;
  Type* Zonk(const Substitution* subst, bool strict, TypeArena* arena) override;

 private:
  Type* target_;
  Substitution* substitution_;
};


class Substitution {
 public:
  explicit Substitution(const Substitution* parent = nullptr) :
      parent_(parent),
      entries_(),
      args_(),
      scope_(nullptr) {}

  ~Substitution() = default;

  Substitution(const Substitution&) = delete;
  Substitution& operator=(const Substitution&) = delete;

  auto parent() const { return parent_; }
  void set_parent(const Substitution* parent) { parent_ = parent; }

  const Map<const TypeVariable*, Type*>& entries() const { return entries_; }
  const Vector<Type*>& args() const { return args_; }
  Scope* scope() const { return scope_; }

  bool Insert(const TypeVariable* var, Type* type) {
    if (var == type) {
      return false;
    }
    if (scope_ == nullptr) {
      scope_ = var->scope();
    }
    if (scope_ == var->scope()) {
      auto [_, success] = entries_.emplace(var, type);
      args_.push_back(type);
      return success;
    } else {
      return false;
    }
  }

  Type* Apply(Type* type, TypeArena* arena, bool savable_this = false) const;

  SubstitutionPtr clone() const {
    xylo_contract(parent() == nullptr);
    auto s = std::make_unique<Substitution>(nullptr);
    s->entries_ = entries_.clone();
    s->args_ = args_.clone();
    s->scope_ = scope_;
    return s;
  }

  void CloseOverMetavars(Scope* scope, TypeArena* arena, Map<TypeMetavar*, Type*>* map);

 private:
  const Substitution* parent_;
  Map<const TypeVariable*, Type*> entries_;
  Vector<Type*> args_;
  Scope* scope_;
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
  std::string Print(const MemberConstraint* type, bool paren, Status* status) const;
  std::string Print(const FunctionType* type, bool paren, Status* status) const;
  std::string Print(const TupleType* type, bool paren, Status* status) const;
  std::string Print(const IntersectionType* type, bool paren, Status* status) const;
  std::string Print(const UnionType* type, bool paren, Status* status) const;
  std::string Print(const TypeVariable* type, bool paren, Status* status) const;
  std::string Print(const TypeMetavar* type, bool paren, Status* status) const;
  std::string Print(const TypeScheme* type, bool paren, Status* status) const;
  std::string Print(const TypeApplication* type, bool paren, Status* status) const;
  std::string Print(const Substitution* subst, Status* status) const;

  std::string PrintConstraints(Status* status) const;

 private:
  TypePrinterOptions options_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_TYPE_H_
