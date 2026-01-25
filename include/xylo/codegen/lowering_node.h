
#ifndef XYLO_CODEGEN_LOWERING_NODE_H_
#define XYLO_CODEGEN_LOWERING_NODE_H_

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>

#include <memory>
#include <utility>

#include "xylo/syntax/ast.h"
#include "xylo/syntax/type.h"
#include "xylo/util/finally.h"
#include "xylo/util/string.h"
#include "xylo/util/vector.h"

namespace xylo {


class LoweringNode;
class ProgramLowerer;
class InterfaceLowerer;
class ClassLowerer;
class FunctionLowerer;
using LoweringNodePtr = std::unique_ptr<LoweringNode>;

class LoweringNode {
 public:
  enum class Kind {
    kProgram,
    kInterface,
    kClass,
    kFunction,
  };

  using TypeVec = Vector<Type*>;
  using MetavarVec = Vector<TypeMetavar*>;

 private:
  LoweringNode(Kind kind, ProgramLowerer* root, LoweringNode* parent, SubstitutionPtr&& subst) :
      kind_(kind),
      root_(root),
      parent_(parent),
      subst_(std::move(subst)),
      children_(),
      interface_decls_(),
      class_decls_(),
      func_decls_(),
      route_registered_(false),
      specialized_classes_(),
      specialized_funcs_(),
      scope_route_map_() {}

 protected:
  LoweringNode(Kind kind, LoweringNode* parent, SubstitutionPtr&& subst) :
      LoweringNode(kind, parent->root(), parent, std::move(subst)) {}

  LoweringNode(Kind kind, ProgramLowerer* root) :
      LoweringNode(kind, root, nullptr, std::make_unique<Substitution>()) {}

 public:
  virtual ~LoweringNode() = default;

  LoweringNode(const LoweringNode&) = delete;
  LoweringNode& operator=(const LoweringNode&) = delete;

  Kind kind() const { return kind_; }
  ProgramLowerer* root() const { return root_; }
  LoweringNode* parent() const { return parent_; }
  const Substitution* subst() const { return subst_.get(); }

  void add_child(LoweringNodePtr&& child) { children_.push_back(std::move(child)); }

  virtual const String& mangled_name() const = 0;
  virtual Scope* scope() const = 0;
  virtual llvm::StructType* scope_data_type() const = 0;

 protected:
  void RegisterDeclaration(Declaration* decl);
  void RegisterInterfaceDecl(InterfaceDeclaration* interface_decl);
  void RegisterClassDecl(ClassDeclaration* class_decl);
  void RegisterFunctionDecl(FunctionDeclaration* func_decl);
  void RegisterScopeRoute(Scope* scope, LoweringNode* route);

  HString TypeArgsKey(const TypeVec& type_args) const;
  SubstitutionPtr ExtendSubstitution(Type* callee, const TypeVec& type_args);

 public:
  FunctionExpression* GetXyloFunction(Symbol* symbol);

  llvm::StructType* GetOrCreateVTableStruct(NominalType* type);
  llvm::StructType* GetOrCreateInstanceStruct(NominalType* type);

  llvm::Function* GetOrBuildMethod(NominalType* type, Identifier* name, const InstantiatedInfo* instantiated_info);
  llvm::Function* GetOrBuildMethod(NominalType* type, Identifier* name, const TypeVec& type_args);

  llvm::Function* GetOrBuildFunction(Symbol* symbol, const InstantiatedInfo* instantiated_info);
  llvm::Function* GetOrBuildFunction(Symbol* symbol, const TypeVec& type_args);

  llvm::StructType* GetVTableStruct(NominalType* type);
  llvm::StructType* GetInstanceStruct(NominalType* type);

  InterfaceLowerer* GetInterfaceLowerer(NominalType* type) { return GetInterfaceLowerer(type, this->subst()); }
  ClassLowerer* GetClassLowerer(NominalType* type) { return GetClassLowerer(type, this->subst()); }

  InterfaceLowerer* GetInterfaceLowerer(NominalType* type, const Substitution* subst);
  ClassLowerer* GetClassLowerer(NominalType* type, const Substitution* subst);

 private:
  Kind kind_;
  ProgramLowerer* root_;
  LoweringNode* parent_;
  SubstitutionPtr subst_;
  Vector<LoweringNodePtr> children_;

  Map<NominalType*, InterfaceDeclaration*> interface_decls_;
  Map<NominalType*, ClassDeclaration*> class_decls_;
  Map<Symbol*, FunctionDeclaration*> func_decls_;

  struct PairStrHash {
    template <typename T>
    size_t operator()(const std::pair<T, HString>& p) const {
      size_t h1 = std::hash<T>()(p.first);
      size_t h2 = HStringHash()(p.second);
      return h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6) + (h1 >> 2));
    }
  };

  bool route_registered_;
  Map<std::pair<NominalType*, HString>, InterfaceLowerer*, PairStrHash> specialized_interfaces_;
  Map<std::pair<NominalType*, HString>, ClassLowerer*, PairStrHash> specialized_classes_;
  Map<std::pair<Symbol*, HString>, FunctionLowerer*, PairStrHash> specialized_funcs_;
  Map<std::pair<Scope*, HString>, LoweringNode*, PairStrHash> scope_route_map_;


  template <typename F>
  auto ProcInDeclarationScope(NominalType* type, const Substitution* subst, F&& proc) -> decltype(proc(this, type)) {
    // move up to LCA scope
    if (!this->scope()->is_outer_than_equal(type->scope())) {
      xylo_contract(parent_ != nullptr);
      return parent_->ProcInDeclarationScope(type, subst, std::forward<F>(proc));
    }

    // move down to type declaration scope
    if (this->scope() != type->scope()) {
      bool specialized = type->is_lexical_instantiation() && this->scope() == type->substitution()->scope()->parent();

      Vector<Type*> type_args;
      TypeArena arena;

      if (specialized) {
        for (auto t : type->substitution()->args()) {
          type_args.push_back(t->Zonk(subst, false, &arena));
        }
      }

      auto args_key = TypeArgsKey(type_args);
      auto key = std::make_pair(type->scope(), std::move(args_key));

      // find route to declaration scope
      auto route_it = scope_route_map_.find(key);
      xylo_contract(route_it != scope_route_map_.end());
      auto route_node = route_it->second;
      xylo_contract(route_node->scope()->parent() == this->scope());

      // move to the child node
      if (specialized) {
        // restore the substitution of the child node
        xylo_contract(type->substitution()->parent() == nullptr);
        type->substitution()->set_parent(subst);
        Finally _([type]() {
          type->substitution()->set_parent(nullptr);
        });

        return route_node->ProcInDeclarationScope(type->origin(), type->substitution(), std::forward<F>(proc));

      } else {
        return route_node->ProcInDeclarationScope(type, subst, std::forward<F>(proc));
      }
    }

    // 'type' is declared in 'this' scope
    return proc(this, type);
  }
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_LOWERING_NODE_H_
