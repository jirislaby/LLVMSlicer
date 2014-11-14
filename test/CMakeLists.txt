set(LLVM_LINK_COMPONENTS core engine asmparser bitreader)
set(LLVM_OPTIONAL_SOURCES field-sensitive-test.cpp dump-points-to.cpp)

add_llvm_executable(field-sensitive-test field-sensitive-test.cpp)
add_llvm_executable(dump-points-to dump-points-to.cpp)

target_link_libraries(field-sensitive-test LLVMSlicer)
target_link_libraries(dump-points-to LLVMSlicer)

add_test(Field-sensitive-test field-sensitive-test)
