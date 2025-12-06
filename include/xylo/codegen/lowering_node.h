
#ifndef XYLO_CODEGEN_LOWERING_NODE_H_
#define XYLO_CODEGEN_LOWERING_NODE_H_

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>

#include <memory>
#include <utility>

#include "xylo/syntax/ast.h"
#include "xylo/syntax/substitution.h"
#include "xylo/syntax/type.h"
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

  LoweringNode(Kind kind, LoweringNode* parent, SubstitutionPtr&& subst) :
      kind_(kind),
      root_(parent->root()),
      parent_(parent),
      subst_(std::move(subst)),
      children_(),
      interface_decls_(),
      class_decls_(),
      func_decls_(),
      specialized_funcs_() {
    parent->add_child(LoweringNodePtr(this));
  }

  LoweringNode(Kind kind, ProgramLowerer* root) :
      kind_(kind),
      root_(root),
      parent_(nullptr),
      subst_(nullptr),
      children_(),
      interface_decls_(),
      class_decls_(),
      func_decls_(),
      specialized_funcs_() {}

  virtual ~LoweringNode() = default;

  LoweringNode(const LoweringNode&) = delete;
  LoweringNode& operator=(const LoweringNode&) = delete;

  Kind kind() const { return kind_; }
  ProgramLowerer* root() const { return root_; }
  LoweringNode* parent() const { return parent_; }
  const Substitution* subst() const { return subst_.get(); }

  void add_child(LoweringNodePtr&& child) { children_.push_back(std::move(child)); }

  virtual const String& mangled_name() const = 0;
  virtual int scope_depth() const = 0;
  virtual llvm::StructType* scope_data_type() const = 0;

 protected:
  void RegisterDeclaration(Declaration* decl);
  void RegisterInterfaceDecl(InterfaceDeclaration* interface_decl);
  void RegisterClassDecl(ClassDeclaration* class_decl);
  void RegisterFunctionDecl(FunctionDeclaration* func_decl);

  FunctionExpression* GetXyloFunction(Symbol* symbol);

  llvm::StructType* GetOrCreateVTableStruct(Symbol* symbol);
  llvm::StructType* GetOrCreateInstanceStruct(Symbol* symbol);
  llvm::Function* GetOrBuildMethod(xylo::NominalType* type, Identifier* name, const TypeVec& type_args);
  llvm::Function* GetOrBuildFunction(Symbol* symbol, const TypeVec& type_args);
  SubstitutionPtr ExtendSubstitution(FunctionDeclaration* func_decl, const TypeVec& type_args);

  llvm::StructType* GetVTableStruct(xylo::NominalType* type);
  llvm::StructType* GetInstanceStruct(xylo::NominalType* type);

 private:
  Kind kind_;
  ProgramLowerer* root_;
  LoweringNode* parent_;
  SubstitutionPtr subst_;
  Vector<LoweringNodePtr> children_;

  Map<Symbol*, InterfaceDeclaration*> interface_decls_;
  Map<Symbol*, ClassDeclaration*> class_decls_;
  Map<Symbol*, FunctionDeclaration*> func_decls_;

  struct SymStrHash {
    size_t operator()(const std::pair<Symbol*, HString>& p) const {
      size_t h1 = std::hash<Symbol*>()(p.first);
      size_t h2 = HStringHash()(p.second);
      return h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6) + (h1 >> 2));
    }
  };

  Map<std::pair<Symbol*, HString>, FunctionLowerer*, SymStrHash> specialized_funcs_;
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_LOWERING_NODE_H_
