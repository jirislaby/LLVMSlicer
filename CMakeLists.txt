cmake_minimum_required(VERSION 2.8)

find_package(LLVM)

# Define add_llvm_* macro's.
include(AddLLVM)

llvm_replace_compiler_option(CMAKE_CXX_FLAGS "-frtti" "-fno-rtti")

add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

find_path(LLVM_SRC_PATH include/llvm/Module.h)
find_path(LLVM_BUILD_PATH include/llvm/Intrinsics.gen)

if (LLVM_SRC_PATH)
  include_directories(${LLVM_SRC_PATH}/include)
endif(LLVM_SRC_PATH)
if (LLVM_BUILD_PATH)
  include_directories(${LLVM_BUILD_PATH}/include)
endif(LLVM_BUILD_PATH)

add_subdirectory(src)
