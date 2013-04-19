add_executable(field-sensitive-test field-sensitive-test.cpp)

llvm_map_components_to_libraries(FST_LLVM_LIBS core engine asmparser bitreader bitwriter)

target_link_libraries(field-sensitive-test LLVMSlicer ${FST_LLVM_LIBS})
