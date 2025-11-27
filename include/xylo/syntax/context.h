
#ifndef XYLO_SYNTAX_CONTEXT_H_
#define XYLO_SYNTAX_CONTEXT_H_

#include <utility>

#include "xylo/syntax/ast.h"
#include "xylo/syntax/identifier.h"
#include "xylo/syntax/type.h"
#include "xylo/util/map.h"

namespace xylo {


class XyloContext {
 public:
  XyloContext() :
      identifier_pool_(),
      class_table_(),
      root_scope_(),
      unit_symbol_(Symbol::Kind::kClass, &root_scope_, InternIdentifier("unit")),
      bool_symbol_(Symbol::Kind::kClass, &root_scope_, InternIdentifier("bool")),
      comparable_symbol_(Symbol::Kind::kClass, &root_scope_, InternIdentifier("Comparable")),
      numeric_symbol_(Symbol::Kind::kClass, &root_scope_, InternIdentifier("Numeric")),
      float_symbol_(Symbol::Kind::kClass, &root_scope_, InternIdentifier("float")),
      int_symbol_(Symbol::Kind::kClass, &root_scope_, InternIdentifier("int")),
      string_symbol_(Symbol::Kind::kClass, &root_scope_, InternIdentifier("string")),
      bottom_type_(),
      unit_type_(unit_symbol_.name()),
      bool_type_(bool_symbol_.name()),
      comparable_type_(comparable_symbol_.name()),
      numeric_type_(numeric_symbol_.name()),
      float_type_(float_symbol_.name()),
      int_type_(int_symbol_.name()),
      string_type_(string_symbol_.name()) {
    // pseudo inheritance relationships
    numeric_type_.add_super(&comparable_type_);
    float_type_.add_super(&numeric_type_);
    int_type_.add_super(&float_type_);
    string_type_.add_super(&comparable_type_);

    // set primitive symbol types
    unit_symbol_.set_type(&unit_type_);
    bool_symbol_.set_type(&bool_type_);
    comparable_symbol_.set_type(&comparable_type_);
    numeric_symbol_.set_type(&numeric_type_);
    float_symbol_.set_type(&float_type_);
    int_symbol_.set_type(&int_type_);
    string_symbol_.set_type(&string_type_);
  }

  ~XyloContext() = default;

  XyloContext(const XyloContext&) = delete;
  XyloContext& operator=(const XyloContext&) = delete;

  Identifier* InternIdentifier(const HStringView& strview) { return identifier_pool_.Intern(strview); }
  Identifier* InternIdentifier(const char* str) { return identifier_pool_.Intern(HStringView(str)); }

  bool RegisterClass(Symbol* symbol, TypePtr&& type) { return class_table_.emplace(symbol, std::move(type)).second; }

  Scope* root_scope() { return &root_scope_; }

  Symbol* unit_symbol() { return &unit_symbol_; }
  Symbol* bool_symbol() { return &bool_symbol_; }
  Symbol* comparable_symbol() { return &comparable_symbol_; }
  Symbol* numeric_symbol() { return &numeric_symbol_; }
  Symbol* float_symbol() { return &float_symbol_; }
  Symbol* int_symbol() { return &int_symbol_; }
  Symbol* string_symbol() { return &string_symbol_; }

  Type* error_type() { return ErrorType::instance(); }
  Type* unit_type() { return &unit_type_; }
  Type* bottom_type() { return &bottom_type_; }
  Type* bool_type() { return &bool_type_; }
  Type* comparable_type() { return &comparable_type_; }
  Type* numeric_type() { return &numeric_type_; }
  Type* float_type() { return &float_type_; }
  Type* int_type() { return &int_type_; }
  Type* string_type() { return &string_type_; }

 private:
  IdentifierPool identifier_pool_;
  Map<Symbol*, TypePtr> class_table_;

  Scope root_scope_;

  Symbol unit_symbol_;
  Symbol bool_symbol_;
  Symbol comparable_symbol_;
  Symbol numeric_symbol_;
  Symbol float_symbol_;
  Symbol int_symbol_;
  Symbol string_symbol_;

  UnionType bottom_type_;
  NominalType unit_type_;
  NominalType bool_type_;
  NominalType comparable_type_;
  NominalType numeric_type_;
  NominalType float_type_;
  NominalType int_type_;
  NominalType string_type_;
};


}  // namespace xylo

#endif  // XYLO_SYNTAX_CONTEXT_H_
