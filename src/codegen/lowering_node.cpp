
#include "xylo/codegen/lowering_node.h"

#include "xylo/codegen/class_lowerer.h"
#include "xylo/codegen/function_lowerer.h"
#include "xylo/codegen/interface_lowerer.h"
#include "xylo/codegen/type_converter.h"

namespace xylo {


static void TypeKey(HString* out, const Type* type, XyloContext* context) {
  switch (type->kind()) {
    case Type::Kind::kNominal: {
      if (type == context->unit_type()) {
        *out += "U";
      } else if (type == context->bool_type()) {
        *out += "B";
      } else if (type == context->int_type()) {
        *out += "I";
      } else if (type == context->float_type()) {
        *out += "F";
      } else if (type == context->string_type()) {
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
      TypeKey(out, func_type->params_type(), context);
      TypeKey(out, func_type->return_type(), context);
      return;
    }

    case Type::Kind::kTuple: {
      auto tuple_type = type->As<TupleType>();
      *out += "t";
      *out += std::to_string(tuple_type->elements().size());
      for (auto elem : tuple_type->elements()) {
        TypeKey(out, elem, context);
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
    case Type::Kind::kMemberReq:
    case Type::Kind::kTyvar:
    case Type::Kind::kMetavar:
    case Type::Kind::kScheme:
      xylo_unreachable();
  }

  xylo_unreachable();
}


static HString TypeArgsKey(const Vector<Type*>& type_args, XyloContext* context) {
  HString key;
  key += std::to_string(type_args.size());
  for (auto t : type_args) {
    TypeKey(&key, t, context);
  }
  return key;
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
}


void LoweringNode::RegisterClassDecl(ClassDeclaration* class_decl) {
  xylo_contract(class_decl->symbol()->type()->kind() == Type::Kind::kNominal);
  class_decls_.emplace(class_decl->symbol(), class_decl);
}


void LoweringNode::RegisterFunctionDecl(FunctionDeclaration* func_decl) {
  func_decls_.emplace(func_decl->symbol(), func_decl);
}


FunctionExpression* LoweringNode::GetXyloFunction(Symbol* symbol) {
  xylo_contract(symbol->kind() == Symbol::Kind::kFunction);

  // check scope
  auto decl_it = func_decls_.find(symbol);
  if (decl_it == func_decls_.end()) {
    xylo_contract(parent_ != nullptr);
    return parent_->GetXyloFunction(symbol);
  }

  // look up function declaration
  return decl_it->second->func();
}


llvm::StructType* LoweringNode::GetOrCreateVTableStruct(Symbol* symbol) {
  xylo_contract(symbol->kind() == Symbol::Kind::kClass);

  auto interface_type = symbol->type()->As<NominalType>();
  auto interface_lowerer = root()->GetInterfaceLowerer(interface_type);

  if (interface_lowerer == nullptr) {
    auto lowerer = this;
    while (true) {
      // check scope
      auto decl_it = lowerer->interface_decls_.find(symbol);
      if (decl_it == lowerer->interface_decls_.end()) {
        xylo_contract(lowerer->parent() != nullptr);
        lowerer = lowerer->parent();
        continue;
      }

      // found
      auto interface_decl = decl_it->second;
      auto ext_subst = std::make_unique<Substitution>(lowerer->subst());
      interface_lowerer = new InterfaceLowerer(lowerer, std::move(ext_subst), interface_decl);
      interface_lowerer->set_interface_name(ChildMangledName(lowerer, symbol, HString()));
      root()->RegisterInterfaceLowerer(interface_type, interface_lowerer);
      break;
    }
  }

  return interface_lowerer->GetOrCreateVTableStruct();
}


llvm::StructType* LoweringNode::GetOrCreateInstanceStruct(Symbol* symbol) {
  xylo_contract(symbol->kind() == Symbol::Kind::kClass);

  auto class_type = symbol->type()->As<NominalType>();
  auto class_lowerer = root()->GetClassLowerer(class_type);

  if (class_lowerer == nullptr) {
    auto lowerer = this;
    while (true) {
      // check scope
      auto decl_it = lowerer->class_decls_.find(symbol);
      if (decl_it == lowerer->class_decls_.end()) {
        xylo_contract(lowerer->parent() != nullptr);
        lowerer = lowerer->parent();
        continue;
      }

      // found
      auto class_decl = decl_it->second;
      auto ext_subst = std::make_unique<Substitution>(lowerer->subst());
      class_lowerer = new ClassLowerer(lowerer, std::move(ext_subst), class_decl);
      class_lowerer->set_class_name(ChildMangledName(lowerer, symbol, HString()));
      root()->RegisterClassLowerer(class_type, class_lowerer);
      break;
    }
  }

  return class_lowerer->GetOrCreateInstanceStruct();
}


llvm::Function* LoweringNode::GetOrBuildMethod(xylo::NominalType* type, Identifier* name, const TypeVec& type_args) {
  return root()->GetClassLowerer(type)->GetOrBuildMethod(name, type_args);
}


llvm::Function* LoweringNode::GetOrBuildFunction(Symbol* symbol, const TypeVec& type_args) {
  xylo_contract(symbol->kind() == Symbol::Kind::kFunction);

  // check scope
  auto decl_it = func_decls_.find(symbol);
  if (decl_it == func_decls_.end()) {
    xylo_contract(parent_ != nullptr);
    return parent_->GetOrBuildFunction(symbol, type_args);
  }

  // look up specialized function lowerer
  auto key = std::make_pair(symbol, TypeArgsKey(type_args, root()->xylo_context()));
  auto sfunc_it = specialized_funcs_.find(key);
  if (sfunc_it == specialized_funcs_.end()) {
    auto func_decl = decl_it->second;
    auto ext_subst = ExtendSubstitution(func_decl, type_args);
    auto func_lowerer = new FunctionLowerer(this, std::move(ext_subst), func_decl->func());
    func_lowerer->set_func_name(ChildMangledName(this, symbol, key.second));
    auto [it, _] = specialized_funcs_.emplace(std::move(key), func_lowerer);
    sfunc_it = it;
  }

  // build
  auto func_lowerer = sfunc_it->second;
  return func_lowerer->GetOrBuildFunction();
}


SubstitutionPtr LoweringNode::ExtendSubstitution(FunctionDeclaration* func_decl, const TypeVec& type_args) {
  auto ext_subst = std::make_unique<Substitution>(subst());

  auto decl_type = func_decl->symbol()->type();
  if (decl_type->kind() == Type::Kind::kScheme) {
    auto type_scheme = decl_type->As<TypeScheme>();
    xylo_contract(type_args.size() == type_scheme->vars().size());

    for (size_t i = 0; i < type_scheme->vars().size(); ++i) {
      ext_subst->insert(type_scheme->vars()[i], type_args[i]);
    }

  } else {
    xylo_contract(type_args.size() == 0);
  }

  return ext_subst;
}


llvm::StructType* LoweringNode::GetVTableStruct(xylo::NominalType* type) {
  return root()->GetInterfaceLowerer(type)->GetOrCreateVTableStruct();
}


llvm::StructType* LoweringNode::GetInstanceStruct(xylo::NominalType* type) {
  return root()->GetClassLowerer(type)->GetOrCreateInstanceStruct();
}


}  // namespace xylo
