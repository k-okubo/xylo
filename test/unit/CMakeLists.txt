include(GoogleTest)

file(GLOB_RECURSE files *_test.cpp)
foreach(file ${files})
  get_filename_component(target ${file} NAME_WE)
  add_executable(${target} ${file})
  target_link_libraries(${target} PRIVATE xylo_syntax xylo_codegen ${LLVM_LIBS} GTest::gtest_main)
  gtest_discover_tests(${target})
endforeach()
