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
  static bool prepareFun(llvm::Function &F);
  static bool prepareModule(llvm::Module &M) {
    bool mod = false;
    for (llvm::Module::iterator I = M.begin(), E = M.end(); I != E; ++I)
      mod |= prepareFun(*I);
    return mod;
  }

private:
  static void replaceInsLoad(llvm::Function &F, llvm::CallInst *CI);
  static void replaceInsStore(llvm::Function &F, llvm::CallInst *CI);
};

}}

#endif
