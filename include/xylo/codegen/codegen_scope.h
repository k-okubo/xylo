
#ifndef XYLO_CODEGEN_CODEGEN_SCOPE_H_
#define XYLO_CODEGEN_CODEGEN_SCOPE_H_

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>

#include <memory>
#include <utility>

#include "xylo/syntax/ast.h"
#include "xylo/syntax/context.h"
#include "xylo/syntax/substitution.h"
#include "xylo/syntax/type.h"
#include "xylo/util/map.h"
#include "xylo/util/string.h"

namespace xylo {


class CodegenScope;
class ModuleLowerer;
class ClassLowerer;
class FunctionLowerer;
using CodegenScopePtr = std::unique_ptr<CodegenScope>;

class CodegenScope {
 public:
  enum class Kind {
    kModule,
    kClass,
    kFunction,
  };

  CodegenScope(Kind kind, CodegenScope* parent, SubstitutionPtr&& type_env) :
      kind_(kind),
      root_(parent->root()),
      parent_(parent),
      type_env_(std::move(type_env)),
      childs_(),
      class_decls_(),
      func_decls_(),
      specialized_funcs_() {
    parent->add_child(CodegenScopePtr(this));
  }

  CodegenScope(Kind kind, ModuleLowerer* root) :
      kind_(kind),
      root_(root),
      parent_(nullptr),
      type_env_(nullptr),
      childs_(),
      class_decls_(),
      func_decls_(),
      specialized_funcs_() {}

  virtual ~CodegenScope() = default;

  CodegenScope(const CodegenScope&) = delete;
  CodegenScope& operator=(const CodegenScope&) = delete;

  Kind kind() const { return kind_; }
  ModuleLowerer* root() const { return root_; }
  CodegenScope* parent() const { return parent_; }
  const Substitution* type_env() const { return type_env_.get(); }

  void add_child(CodegenScopePtr&& child) { childs_.push_back(std::move(child)); }

  virtual const String& mangled_name() const = 0;
  virtual int scope_depth() const = 0;
  virtual llvm::StructType* scope_data_type() const = 0;

 protected:
  void RegisterDeclaration(Declaration* decl);
  void RegisterClass(ClassDeclaration* class_decl);
  void RegisterFunction(FunctionDeclaration* func_decl);

  llvm::StructType* GetOrCreateStruct(Symbol* symbol);

  FunctionExpression* GetXyloFunction(Symbol* symbol);
  llvm::Function* GetOrBuildFunction(Symbol* symbol, const Vector<Type*>& type_args);
  SubstitutionPtr ExtendTypeEnv(FunctionDeclaration* func_decl, const Vector<Type*>& type_args);

  struct SymStrHash {
    size_t operator()(const std::pair<Symbol*, HString>& p) const {
      size_t h1 = std::hash<Symbol*>()(p.first);
      size_t h2 = HStringHash()(p.second);
      return h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6) + (h1 >> 2));
    }
  };

 private:
  Kind kind_;
  ModuleLowerer* root_;
  CodegenScope* parent_;
  SubstitutionPtr type_env_;
  Vector<CodegenScopePtr> childs_;

  Map<Symbol*, ClassDeclaration*> class_decls_;
  Map<Symbol*, FunctionDeclaration*> func_decls_;
  Map<std::pair<Symbol*, HString>, FunctionLowerer*, SymStrHash> specialized_funcs_;
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_CODEGEN_SCOPE_H_
