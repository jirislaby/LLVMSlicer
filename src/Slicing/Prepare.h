//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef SLIC_PREPARE_H
#define SLIC_PREPARE_H

#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"

namespace llvm { namespace slicing {

class Prepare {
public:
  static void prepareFun(llvm::Function &F);
  static void prepareModule(llvm::Module &M) {
    for (llvm::Module::iterator I = M.begin(), E = M.end(); I != E; ++I)
      prepareFun(*I);
  }

private:
  static void replaceInsCheck(llvm::Function &F, llvm::CallInst *CI);
  static void replaceInsTrans(llvm::Function &F, llvm::CallInst *CI);
};

}}

#endif
