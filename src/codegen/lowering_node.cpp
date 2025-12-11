
#include "xylo/codegen/lowering_node.h"

#include "xylo/codegen/class_lowerer.h"
#include "xylo/codegen/function_lowerer.h"
#include "xylo/codegen/interface_lowerer.h"
#include "xylo/codegen/mangler.h"
#include "xylo/codegen/type_converter.h"

namespace xylo {


HString LoweringNode::TypeArgsKey(const TypeVec& type_args) const {
  Mangler mangler(root()->xylo_context());
  return mangler.TypeArgsKey(type_args);
}


static String ChildMangledName(LoweringNode* parent, Symbol* symbol, const HString& type_args_key) {
  String mangled;

  mangled += parent->mangled_name();
  mangled += "__";
  mangled += std::to_string(symbol->name()->str().size());
  mangled += symbol->name()->str();

  if (type_args_key.size() > 1) {
    mangled += "_";
    mangled += type_args_key;
  }

  return mangled;
}


void LoweringNode::RegisterDeclaration(Declaration* decl) {
  switch (decl->kind()) {
    case Declaration::Kind::kInterface:
      RegisterInterfaceDecl(decl->As<InterfaceDeclaration>());
      break;

    case Declaration::Kind::kClass:
      RegisterClassDecl(decl->As<ClassDeclaration>());
      break;

    case Declaration::Kind::kFunction:
      RegisterFunctionDecl(decl->As<FunctionDeclaration>());
      break;
  }
}


void LoweringNode::RegisterInterfaceDecl(InterfaceDeclaration* interface_decl) {
  xylo_contract(interface_decl->symbol()->type()->kind() == Type::Kind::kNominal);
  interface_decls_.emplace(interface_decl->symbol(), interface_decl);

  if (!route_registered_ && parent_ != nullptr) {
    parent_->RegisterScopeRoute(this->scope(), this);
    route_registered_ = true;
  }
}


void LoweringNode::RegisterClassDecl(ClassDeclaration* class_decl) {
  xylo_contract(class_decl->symbol()->type()->kind() == Type::Kind::kNominal);
  class_decls_.emplace(class_decl->symbol(), class_decl);

  if (!route_registered_ && parent_ != nullptr) {
    parent_->RegisterScopeRoute(this->scope(), this);
    route_registered_ = true;
  }
}


void LoweringNode::RegisterFunctionDecl(FunctionDeclaration* func_decl) {
  func_decls_.emplace(func_decl->symbol(), func_decl);
}


void LoweringNode::RegisterScopeRoute(Scope* scope, LoweringNode* route) {
  auto key = std::make_pair(scope, TypeArgsKey(route->subst()->args()));
  auto [_, inserted] = scope_route_map_.emplace(std::move(key), route);
  xylo_contract(inserted);

  if (parent_ != nullptr) {
    parent_->RegisterScopeRoute(scope, this);
  }
}


FunctionExpression* LoweringNode::GetXyloFunction(Symbol* symbol) {
  xylo_contract(symbol->kind() == Symbol::Kind::kFunction);

  // move to symbol declaration scope
  if (this->scope() != symbol->scope()) {
    xylo_contract(parent_ != nullptr);
    return parent_->GetXyloFunction(symbol);
  }

  auto decl_it = func_decls_.find(symbol);
  xylo_contract(decl_it != func_decls_.end());
  return decl_it->second->func();
}


llvm::StructType* LoweringNode::GetOrCreateVTableStruct(Symbol* symbol) {
  xylo_contract(symbol->kind() == Symbol::Kind::kClass);

  // move to symbol declaration scope
  if (this->scope() != symbol->scope()) {
    xylo_contract(parent_ != nullptr);
    return parent_->GetOrCreateVTableStruct(symbol);
  }

  // look up specialized lowerer
  auto key = std::make_pair(symbol->type()->As<NominalType>(), HString());
  auto sint_it = specialized_interfaces_.find(key);
  if (sint_it == specialized_interfaces_.end()) {
    // create new lowerer
    auto decl_it = interface_decls_.find(symbol);
    xylo_contract(decl_it != interface_decls_.end());
    auto interface_decl = decl_it->second;

    auto ext_subst = ExtendSubstitution(interface_decl->symbol()->type(), {});
    auto interface_lowerer = new InterfaceLowerer(this, std::move(ext_subst), interface_decl);
    interface_lowerer->set_interface_name(ChildMangledName(this, symbol, key.second));

    add_child(LoweringNodePtr(interface_lowerer));
    auto [it, _] = specialized_interfaces_.emplace(std::move(key), interface_lowerer);
    sint_it = it;
  }

  // create vtable struct
  auto interface_lowerer = sint_it->second;
  return interface_lowerer->GetOrCreateVTableStruct();
}


llvm::StructType* LoweringNode::GetOrCreateInstanceStruct(Symbol* symbol) {
  xylo_contract(symbol->kind() == Symbol::Kind::kClass);

  // move to symbol declaration scope
  if (this->scope() != symbol->scope()) {
    xylo_contract(parent_ != nullptr);
    return parent_->GetOrCreateInstanceStruct(symbol);
  }

  // look up specialized lowerer
  auto key = std::make_pair(symbol->type()->As<NominalType>(), HString());
  auto sclass_it = specialized_classes_.find(key);
  if (sclass_it == specialized_classes_.end()) {
    // create new lowerer
    auto decl_it = class_decls_.find(symbol);
    xylo_contract(decl_it != class_decls_.end());
    auto class_decl = decl_it->second;

    auto ext_subst = ExtendSubstitution(class_decl->symbol()->type(), {});
    auto class_lowerer = new ClassLowerer(this, std::move(ext_subst), class_decl);
    class_lowerer->set_class_name(ChildMangledName(this, symbol, key.second));

    add_child(LoweringNodePtr(class_lowerer));
    auto [it, _] = specialized_classes_.emplace(std::move(key), class_lowerer);
    sclass_it = it;
  }

  // create instance struct
  auto class_lowerer = sclass_it->second;
  return class_lowerer->GetOrCreateInstanceStruct();
}


llvm::Function* LoweringNode::GetOrBuildMethod(NominalType* type, Identifier* name, const TypeVec& type_args) {
  return GetClassLowerer(type, subst())->GetOrBuildMethod(name, type_args);
}


llvm::Function* LoweringNode::GetOrBuildFunction(Symbol* symbol, const TypeVec& type_args) {
  xylo_contract(symbol->kind() == Symbol::Kind::kFunction);

  // move to symbol declaration scope
  if (this->scope() != symbol->scope()) {
    xylo_contract(parent_ != nullptr);
    return parent_->GetOrBuildFunction(symbol, type_args);
  }

  // look up specialized lowerer
  auto key = std::make_pair(symbol, TypeArgsKey(type_args));
  auto sfunc_it = specialized_funcs_.find(key);
  if (sfunc_it == specialized_funcs_.end()) {
    // create new lowerer
    auto decl_it = func_decls_.find(symbol);
    xylo_contract(decl_it != func_decls_.end());
    auto func_decl = decl_it->second;

    auto ext_subst = ExtendSubstitution(func_decl->symbol()->type(), type_args);
    auto func_lowerer = new FunctionLowerer(this, std::move(ext_subst), func_decl->func());
    func_lowerer->set_func_name(ChildMangledName(this, symbol, key.second));

    add_child(LoweringNodePtr(func_lowerer));
    auto [it, _] = specialized_funcs_.emplace(std::move(key), func_lowerer);
    sfunc_it = it;
  }

  // build function
  auto func_lowerer = sfunc_it->second;
  return func_lowerer->GetOrBuildFunction();
}


SubstitutionPtr LoweringNode::ExtendSubstitution(Type* callee, const TypeVec& type_args) {
  auto ext_subst = std::make_unique<Substitution>(this->subst());

  if (callee->kind() == Type::Kind::kScheme) {
    auto type_scheme = callee->As<TypeScheme>();
    xylo_contract(type_args.size() == type_scheme->vars().size());

    for (size_t i = 0; i < type_scheme->vars().size(); ++i) {
      xylo_check(ext_subst->Insert(type_scheme->vars()[i], type_args[i]));
    }

  } else {
    xylo_contract(type_args.size() == 0);
  }

  return ext_subst;
}


llvm::StructType* LoweringNode::GetVTableStruct(NominalType* type) {
  return GetInterfaceLowerer(type, this->subst())->GetOrCreateVTableStruct();
}


llvm::StructType* LoweringNode::GetInstanceStruct(NominalType* type) {
  return GetClassLowerer(type, this->subst())->GetOrCreateInstanceStruct();
}


InterfaceLowerer* LoweringNode::GetInterfaceLowerer(NominalType* type, const Substitution* subst) {
  return ProcInDeclarationScope(type, subst, [](LoweringNode* node, NominalType* type) {
    xylo_contract(type->substitution() == nullptr);  // not supported generic class yet
    auto key = std::make_pair(type, HString());

    auto sint_it = node->specialized_interfaces_.find(key);
    xylo_contract(sint_it != node->specialized_interfaces_.end());
    auto interface_lowerer = sint_it->second;

    return interface_lowerer;
  });
}


ClassLowerer* LoweringNode::GetClassLowerer(NominalType* type, const Substitution* subst) {
  return ProcInDeclarationScope(type, subst, [](LoweringNode* node, NominalType* type) {
    xylo_contract(type->substitution() == nullptr);  // not supported generic class yet
    auto key = std::make_pair(type, HString());

    auto sclass_it = node->specialized_classes_.find(key);
    xylo_contract(sclass_it != node->specialized_classes_.end());
    auto class_lowerer = sclass_it->second;

    return class_lowerer;
  });
}


}  // namespace xylo
