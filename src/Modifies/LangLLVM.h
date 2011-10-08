// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef MODIFIES_LANGLLVM_H
#define MODIFIES_LANGLLVM_H

#include "Modifies.h"
#include "../Languages/LLVM.h"

namespace llvm { namespace mods {

  template<>
  struct ProgramFunction<LLVM> {
      typedef llvm::Function const *Type;
  };

  template<>
  struct ProgramVariable<LLVM>
  {
      typedef llvm::Value const*
              Type;
  };

  template<bool IsMayAnalysis, bool IsInterproc>
  struct LLVMProgramStructure : public FunctionWrites<LLVM,
        llvm::AnalysisProperties<IsMayAnalysis,IsInterproc,
                                    false,false,false,false> > {
    typedef llvm::AnalysisProperties<IsMayAnalysis,IsInterproc,
                               false,false,false,false>
            AnalysisProperties;
    typedef FunctionWrites<LLVM,AnalysisProperties> Base;

    LLVMProgramStructure(Module &M);

    bool isLocalToFunction(ProgramVariable<LLVM>::Type const& V,
                        ProgramFunction<LLVM>::Type const& F) const
    { return llvm::isLocalToFunction(V,F); }

    bool isConstantValue(ProgramVariable<LLVM>::Type const& V) const
    { return llvm::isConstantValue(V); }
  };

  template<bool IsMayAnalysis, bool IsInterproc>
  struct ProgramStructure<LLVM,
      llvm::AnalysisProperties<IsMayAnalysis,IsInterproc,false,false,false,
                                  false> > {
      typedef LLVMProgramStructure<IsMayAnalysis,IsInterproc> Type;
  };

}}

namespace llvm { namespace mods {

  template<bool IsMayAnalysis, bool IsInterproc>
  LLVMProgramStructure<IsMayAnalysis,IsInterproc>::LLVMProgramStructure(
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
