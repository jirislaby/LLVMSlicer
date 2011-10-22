// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "llvm/Instruction.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"

#include "PredefContainers.h"

using namespace llvm;

namespace llvm { namespace mods {

  ProgramStructure::ProgramStructure(Module &M) {
    for (Module::iterator f = M.begin(); f != M.end(); ++f)
      if (!f->isDeclaration() && !memoryManStuff(f))
        for (inst_iterator i = inst_begin(*f); i != inst_end(*f); ++i)
          if (const StoreInst * s = dyn_cast<const StoreInst>(&*i)) {
            Value const* const l = s->getOperand(1);
	    this->getContainer()[&*f].push_back(
		typename ProgramStructure::Command(
		  hasExtraReference(l) ? CMD_VAR : CMD_DREF_VAR,l));
          }
  }

}}
