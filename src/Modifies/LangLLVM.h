// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef MODIFIES_LANGLLVM_H
#define MODIFIES_LANGLLVM_H

#include "Modifies.h"
#include "../Languages/LLVM.h"

namespace llvm { namespace mods {

  struct LLVMProgramStructure : public FunctionWrites<LLVM> {
    typedef FunctionWrites<LLVM> Base;

    LLVMProgramStructure(Module &M);

    bool isLocalToFunction(const llvm::Value *const& V,
                        const llvm::Function *const& F) const
    { return llvm::isLocalToFunction(V,F); }

    bool isConstantValue(const llvm::Value *const& V) const
    { return llvm::isConstantValue(V); }
  };

  template<>
  struct ProgramStructure<LLVM> {
      typedef LLVMProgramStructure Type;
  };

}}

namespace llvm { namespace mods {

  LLVMProgramStructure::LLVMProgramStructure(
          Module &M)
      : Base() {
    for (llvm::Module::iterator f = M.begin(); f != M.end(); ++f)
      if (!f->isDeclaration() && !memoryManStuff(f))
        for (llvm::inst_iterator i = llvm::inst_begin(*f);
             i != llvm::inst_end(*f); i++)
          if (const llvm::StoreInst * s =
                llvm::dyn_cast<const llvm::StoreInst>(&*i)) {
            llvm::Value const* const l = s->getOperand(1);
                this->getContainer()[&*f].push_back(
                    typename LLVMProgramStructure::Command(
                      hasExtraReference(l) ? CMD_VAR : CMD_DREF_VAR,l));
          }
  }

}}

#endif
