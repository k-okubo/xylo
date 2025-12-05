
#ifndef XYLO_CODEGEN_FUNCTION_LOWERER_H_
#define XYLO_CODEGEN_FUNCTION_LOWERER_H_

#include <llvm/IR/IRBuilder.h>

#include <utility>

#include "xylo/codegen/declaration_lowerer.h"

namespace xylo {


class FunctionLowerer : public DeclarationLowerer {
 public:
  FunctionLowerer(LoweringNode* parent, SubstitutionPtr&& type_env, FunctionExpression* xylo_func) :
      DeclarationLowerer(Kind::kFunction, parent, std::move(type_env)),
      xylo_func_(xylo_func),
      func_name_(),
      llvm_func_(nullptr),
      builder_(llvm_context()),
      variable_value_map_(),
      closure_env_ptr_(nullptr),
      heap_frame_type_(nullptr),
      heap_frame_ptr_(nullptr) {}

  ~FunctionLowerer() = default;

  FunctionExpression* xylo_func() const { return xylo_func_; }
  const String& func_name() const { return func_name_; }
  void set_func_name(String&& name) { func_name_ = std::move(name); }

  const String& mangled_name() const override { return func_name_; }
  int scope_depth() const override { return xylo_func()->scope()->depth(); }
  llvm::StructType* scope_data_type() const override { return heap_frame_type(); }

  llvm::Function* GetOrBuildFunction();
  llvm::Function* CreatePrototype();
  void BuildBody(llvm::Function* function);

 protected:
  void BuildHeapFrame();

  llvm::Value* BuildBlock(Block* block);
  llvm::Value* BuildStatement(Statement* stmt);
  llvm::Value* BuildExpressionStatement(ExpressionStatement* stmt);
  llvm::Value* BuildLetStatement(LetStatement* stmt);
  llvm::Value* BuildVarStatement(VarStatement* stmt);
  llvm::Value* BuildReturnStatement(ReturnStatement* stmt);

  llvm::Value* BuildExpression(Expression* expr);
  llvm::Value* BuildNullExpression(NullExpression* expr);
  llvm::Value* BuildLiteralExpression(LiteralExpression* expr);
  llvm::Value* BuildThisExpression(ThisExpression* expr);
  llvm::Value* BuildIdentifierExpression(IdentifierExpression* expr, llvm::Value** out_closure_env);
  llvm::Value* BuildClosureEnvIdentifier(IdentifierExpression* expr, llvm::Value** out_closure_env);
  llvm::Value* BuildLocalValueIdentifier(IdentifierExpression* expr, llvm::Value** out_closure_env);
  llvm::Value* BuildFunctionIdentifier(IdentifierExpression* expr, llvm::Value** out_closure_env);
  llvm::Value* BuildFunctionExpression(FunctionExpression* expr);
  llvm::Value* BuildApplyExpression(ApplyExpression* expr);
  llvm::Value* BuildUnaryExpression(UnaryExpression* expr);
  llvm::Value* BuildBinaryExpression(BinaryExpression* expr);
  llvm::Value* BuildConditionalExpression(ConditionalExpression* expr);
  llvm::Value* BuildConstructExpression(ConstructExpression* expr);
  llvm::Value* BuildProjectionExpression(ProjectionExpression* expr, llvm::Value** out_closure_env);
  llvm::Value* BuildClassProjection(ProjectionExpression* expr, NominalType* type, llvm::Value** out_closure_env);
  llvm::Value* BuildInterfaceProjection(ProjectionExpression* expr, NominalType* type, llvm::Value** out_closure_env);
  llvm::Value* BuildBlockExpression(BlockExpression* expr);

  void BuildExpressionInitializer(ExpressionInitializer* init, xylo::Type* var_type, llvm::Value* ptr);
  void BuildObjectInitializer(ObjectInitializer* init, xylo::NominalType* obj_type, llvm::Value* ptr);
  void BuildFieldEntry(FieldEntry* entry, xylo::Type* var_type, llvm::Value* ptr);

 protected:
  llvm::Type* ZonkAndConvert(xylo::Type* type, bool function_as_pointer);
  llvm::Value* ZonkAndAdjustType(llvm::Value* value, xylo::Type* from_type, xylo::Type* to_type);
  llvm::Value* AdjustType(llvm::Value* value, xylo::Type* zonked_from_type, xylo::Type* zonked_to_type);

  llvm::Value* closure_env_ptr() const { return closure_env_ptr_; }
  void set_closure_env_ptr(llvm::Value* ptr) { closure_env_ptr_ = ptr; }

  llvm::Value* heap_frame_ptr() const { return heap_frame_ptr_; }
  void set_heap_frame_ptr(llvm::Value* ptr) { heap_frame_ptr_ = ptr; }

  llvm::StructType* heap_frame_type() const { return heap_frame_type_; }
  void set_heap_frame_type(llvm::StructType* type) { heap_frame_type_ = type; }

  llvm::Value* null_ptr() const { return llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(llvm_context())); }

  llvm::Value* BuildHeapAlloc(llvm::Type* type);
  llvm::Value* StoreCapturedValue(Symbol* symbol, llvm::Value* value);
  llvm::Value* StoreToHeapFrame(int index, llvm::Value* value);
  llvm::Value* LoadThisPointer();
  std::pair<llvm::Value*, llvm::StructType*> LoadClosureEnvironmentPtr(Symbol* symbol);
  std::pair<llvm::Value*, llvm::Type*> LoadClosureEnvironmentValuePtr(Symbol* symbol);

  llvm::Value* BuildClosurePointer(llvm::Value* function, llvm::Value* env_ptr);
  llvm::Value* LoadFunctionPtrFromClosurePointer(llvm::Value* closure_ptr);
  llvm::Value* LoadEnvironmentPtrFromClosurePointer(llvm::Value* closure_ptr);

  llvm::Value* BuildInterfacePointer(llvm::Value* obj_ptr, llvm::Value* vtable_ptr);
  llvm::Value* LoadObjectPtrFromInterfacePointer(llvm::Value* interface_ptr);
  llvm::Value* LoadVTablePtrFromInterfacePointer(llvm::Value* interface_ptr);

 private:
  FunctionExpression* xylo_func_;
  String func_name_;
  llvm::Function* llvm_func_;

  llvm::IRBuilder<> builder_;
  Map<Symbol*, llvm::Value*> variable_value_map_;

  llvm::Value* closure_env_ptr_;
  llvm::StructType* heap_frame_type_;
  llvm::Value* heap_frame_ptr_;
};


}  // namespace xylo

#endif  // XYLO_CODEGEN_FUNCTION_LOWERER_H_
