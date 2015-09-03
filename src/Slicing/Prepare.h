// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef SLICING_PREPARE_H
#define SLICING_PREPARE_H

#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"

static inline const llvm::ConstantArray *getInitFuns(const llvm::Module &M) {
  const llvm::GlobalVariable *initFunsVar =
      M.getGlobalVariable("__ai_init_functions", true);

  if (!initFunsVar)
    return NULL;

  const llvm::ConstantArray *initFuns =
      llvm::dyn_cast<llvm::ConstantArray>(initFunsVar->getInitializer());
  return initFuns;
}

#endif
