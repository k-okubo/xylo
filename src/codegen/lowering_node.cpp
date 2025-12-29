
#include "xylo/codegen/lowering_node.h"

#include "xylo/codegen/class_lowerer.h"
#include "xylo/codegen/function_lowerer.h"
#include "xylo/codegen/interface_lowerer.h"
#include "xylo/codegen/mangler.h"
#include "xylo/codegen/type_converter.h"

namespace xylo {


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
  interface_decls_.emplace(interface_decl->interface_type(), interface_decl);

  if (!route_registered_ && parent_ != nullptr) {
    parent_->RegisterScopeRoute(this->scope(), this);
    route_registered_ = true;
  }
}


void LoweringNode::RegisterClassDecl(ClassDeclaration* class_decl) {
  class_decls_.emplace(class_decl->class_type(), class_decl);

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


FunctionExpression* LoweringNode::GetXyloFunction(Symbol* symbol) {
  xylo_contract(symbol->kind() == Symbol::Kind::kFunc);

  // move to symbol declaration scope
  if (this->scope() != symbol->scope()) {
    xylo_contract(parent_ != nullptr);
    return parent_->GetXyloFunction(symbol);
  }

  auto decl_it = func_decls_.find(symbol);
  xylo_contract(decl_it != func_decls_.end());
  return decl_it->second->func();
}


llvm::StructType* LoweringNode::GetOrCreateVTableStruct(NominalType* type) {
  return GetInterfaceLowerer(type, this->subst())->GetOrCreateVTableStruct();
}


llvm::StructType* LoweringNode::GetOrCreateInstanceStruct(NominalType* type) {
  return GetClassLowerer(type, this->subst())->GetOrCreateInstanceStruct();
}


llvm::Function* LoweringNode::GetOrBuildMethod(NominalType* type, Identifier* name,
                                               const InstantiatedInfo* instantiated_info) {
  if (!instantiated_info) {
    return GetOrBuildMethod(type, name, TypeVec{});
  }

  TypeArena arena;
  TypeVec type_args;

  for (auto var : instantiated_info->vars) {
    auto type = var->Zonk(subst(), false, &arena);
    type_args.push_back(type);
  }

  return GetOrBuildMethod(type, name, type_args);
}


llvm::Function* LoweringNode::GetOrBuildMethod(NominalType* type, Identifier* name, const TypeVec& type_args) {
  return GetClassLowerer(type, subst())->GetOrBuildMethod(name, type_args);
}


llvm::Function* LoweringNode::GetOrBuildFunction(Symbol* symbol, const InstantiatedInfo* instantiated_info) {
  if (!instantiated_info) {
    return GetOrBuildFunction(symbol, TypeVec{});
  }

  TypeArena arena;
  TypeVec type_args;

  for (auto var : instantiated_info->vars) {
    auto type = var->Zonk(subst(), false, &arena);
    type_args.push_back(type);
  }

  return GetOrBuildFunction(symbol, type_args);
}


llvm::Function* LoweringNode::GetOrBuildFunction(Symbol* symbol, const TypeVec& type_args) {
  xylo_contract(symbol->kind() == Symbol::Kind::kFunc);

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


llvm::StructType* LoweringNode::GetVTableStruct(NominalType* type) {
  return GetInterfaceLowerer(type, this->subst())->GetOrCreateVTableStruct();
}


llvm::StructType* LoweringNode::GetInstanceStruct(NominalType* type) {
  return GetClassLowerer(type, this->subst())->GetOrCreateInstanceStruct();
}


InterfaceLowerer* LoweringNode::GetInterfaceLowerer(NominalType* type, const Substitution* subst) {
  xylo_contract(type->category() == NominalType::Category::kInterface);

  return ProcInDeclarationScope(type, subst, [](LoweringNode* node, NominalType* type) {
    NominalType* origin_type;
    Vector<Type*> type_args;
    TypeArena arena;

    if (type->substitution() != nullptr) {
      origin_type = type->origin();
      for (auto t : type->substitution()->args()) {
        type_args.push_back(t);
      }
    } else {
      origin_type = type;
    }
    xylo_contract(origin_type->origin() == nullptr);
    xylo_contract(origin_type->substitution() == nullptr);

    auto key = std::make_pair(type, node->TypeArgsKey(type_args));
    auto sint_it = node->specialized_interfaces_.find(key);
    if (sint_it == node->specialized_interfaces_.end()) {
      // create new lowerer
      auto decl_it = node->interface_decls_.find(origin_type);
      xylo_contract(decl_it != node->interface_decls_.end());
      auto interface_decl = decl_it->second;

      auto ext_subst = node->ExtendSubstitution(interface_decl->symbol()->type(), type_args);
      auto interface_lowerer = new InterfaceLowerer(node, std::move(ext_subst), interface_decl);
      interface_lowerer->set_interface_name(ChildMangledName(node, interface_decl->symbol(), key.second));

      node->add_child(LoweringNodePtr(interface_lowerer));
      auto [it, _] = node->specialized_interfaces_.emplace(std::move(key), interface_lowerer);
      sint_it = it;
    }

    auto interface_lowerer = sint_it->second;
    return interface_lowerer;
  });
}


ClassLowerer* LoweringNode::GetClassLowerer(NominalType* type, const Substitution* subst) {
  xylo_contract(type->category() == NominalType::Category::kClass);

  return ProcInDeclarationScope(type, subst, [](LoweringNode* node, NominalType* type) {
    NominalType* origin_type;
    Vector<Type*> type_args;
    TypeArena arena;

    if (type->substitution() != nullptr) {
      origin_type = type->origin();
      for (auto t : type->substitution()->args()) {
        type_args.push_back(t);
      }
    } else {
      origin_type = type;
    }
    xylo_contract(origin_type->origin() == nullptr);
    xylo_contract(origin_type->substitution() == nullptr);

    auto key = std::make_pair(origin_type, node->TypeArgsKey(type_args));
    auto sclass_it = node->specialized_classes_.find(key);
    if (sclass_it == node->specialized_classes_.end()) {
      // create new lowerer
      auto decl_it = node->class_decls_.find(origin_type);
      xylo_contract(decl_it != node->class_decls_.end());
      auto class_decl = decl_it->second;

      auto ext_subst = node->ExtendSubstitution(class_decl->symbol()->type(), type_args);
      auto class_lowerer = new ClassLowerer(node, std::move(ext_subst), class_decl);
      class_lowerer->set_class_name(ChildMangledName(node, class_decl->symbol(), key.second));

      node->add_child(LoweringNodePtr(class_lowerer));
      auto [it, _] = node->specialized_classes_.emplace(std::move(key), class_lowerer);
      sclass_it = it;
    }

    auto class_lowerer = sclass_it->second;
    return class_lowerer;
  });
}


}  // namespace xylo
