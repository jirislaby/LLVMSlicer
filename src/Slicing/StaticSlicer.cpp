// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "llvm/Instructions.h"
#include "llvm/Function.h"
#include "llvm/Pass.h"
#include "llvm/Value.h"

#include "../Callgraph/Callgraph.h"
#include "../Modifies/Modifies.h"
#include "../Modifies/AlgoDumbSpeedy.h"
#include "../PointsTo/AlgoAndersen.h"
#include "../PointsTo/PointsTo.h"
#include "StaticSlicer.h"

namespace llvm { namespace slicing { namespace detail {

    void fillParamsToArgs(llvm::CallInst const* const C,
                          llvm::Function const* const F,
                          ParamsToArgs& toArgs)
    {
        llvm::Function::const_arg_iterator p = F->arg_begin();
        std::size_t a = 0;
        for ( ; a < C->getNumArgOperands(); ++a, ++p)
        {
            llvm::Value const* const P = &*p;
            llvm::Value const* const A = C->getArgOperand(a);
            if (!llvm::isa<llvm::Constant>(A))
                toArgs[P] = A;
        }
    }

}}}

namespace llvm { namespace slicing {

    bool StaticSlicer::sliceModule()
    {
      bool modified = false;
      for (Slicers::iterator s = slicers.begin(); s != slicers.end(); ++s)
	modified |= s->second->slice();
      return modified;
    }
#if 0
    void StaticSlicer::dump(std::ostream& ostr) const
    {
        using monty::codespy::dump;

        for (Slicers::const_iterator s = slicers.begin();
                s != slicers.end(); ++s)
        {
            ostr << "Function ";
            codespy::dump(ostr,s->first);
            ostr << std::endl;
            s->second->dump(ostr);
            ostr << std::endl;
        }
    }
#endif
}}

using namespace llvm;

namespace {
  class Slicer : public ModulePass {
    public:
      static char ID;

      Slicer() : ModulePass(ID) {}

      virtual bool runOnModule(Module &M);

      void getAnalysisUsage(AnalysisUsage &AU) const {
        AU.addRequired<PostDominatorTree>();
        AU.addRequired<PostDominanceFrontier>();
      }
/*    private:
      template<typename PointsToSets, typename ModifiesSets>
      bool runOnFunction(Function &F, const PointsToSets &PS,
                         const ModifiesSets &MOD);
      void findInitialCriterion(Function &F, FunctionStaticSlicer &ss);*/
  };
}

static RegisterPass<Slicer> X("slice-inter", "Slices the code interprocedurally");
char Slicer::ID;

bool Slicer::runOnModule(Module &M) {
  ptr::PointsToSets<ptr::ANDERSEN>::Type PS;
  {
    ptr::ProgramStructure P(M);
    computePointsToSets(P, PS);
  }

  callgraph::Callgraph CG(M, PS);

  mods::Modifies<mods::DUMB_SPEEDY>::Type MOD;
  {
    mods::ProgramStructure P1(M);
    computeModifies(P1, CG, PS, MOD);
  }

  slicing::StaticSlicer SS(this, M, PS, MOD);
//  SS.computeSlice(I,V);
  return SS.sliceModule();
}
