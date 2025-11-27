add_subdirectory(codegen)
add_subdirectory(syntax)

add_executable(xylo xylo.cpp)
target_link_libraries(xylo PRIVATE xylo_syntax)
target_link_libraries(xylo PRIVATE xylo_codegen)
target_link_libraries(xylo PRIVATE ${LLVM_LIBS})

add_executable(sandbox sandbox.cpp)
