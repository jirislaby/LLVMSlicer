// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "llvm/Instructions.h"
#include "llvm/Function.h"
#include "llvm/Pass.h"
#include "llvm/Value.h"

#include "../Callgraph/Callgraph.h"
#include "../Modifies/Modifies.h"
#include "../PointsTo/PointsTo.h"
#include "StaticSlicer.h"

using namespace llvm;

namespace llvm { namespace slicing { namespace detail {

    void fillParamsToArgs(CallInst const* const C,
                          Function const* const F,
                          ParamsToArgs& toArgs)
    {
        Function::const_arg_iterator p = F->arg_begin();
        std::size_t a = 0;
        for ( ; a < C->getNumArgOperands(); ++a, ++p)
        {
            Value const* const P = &*p;
            Value const* const A = C->getArgOperand(a);
            if (!isConstantValue(A))
                toArgs[P] = A;
        }
    }

}}}

namespace llvm { namespace slicing {

    void StaticSlicer::buildDicts(const ptr::PointsToSets &PS)
    {
        typedef Module::iterator FunctionsIter;
        for (FunctionsIter f = module.begin(); f != module.end(); ++f)
            if (!f->isDeclaration() && !memoryManStuff(&*f))
                for (inst_iterator i = inst_begin(*f);
                        i != inst_end(*f); i++)
                    if (CallInst const* c =
                            dyn_cast<CallInst const>(&*i)) {
                        if (isInlineAssembly(c)) {
                            errs() << "ERROR: Inline assembler detected in " <<
                                f->getName() << ", skipping\n";
                            continue;
                        }
			typedef std::vector<const Function *> FunCon;
			FunCon G;
			getCalledFunctions(c, PS, std::back_inserter(G));

                        for (FunCon::const_iterator g = G.begin();
					g != G.end(); ++g) {
                            Function const* const h = *g;
                            if (!memoryManStuff(h) && !h->isDeclaration()) {
                                funcsToCalls.insert(std::make_pair(h, c));
                                callsToFuncs.insert(std::make_pair(c, h));
                            }
                        }
                    }
    }

    void StaticSlicer::computeSlice() {
        typedef SmallVector<const Function *, 20> WorkSet;
        WorkSet Q(initFuns);

        while (!Q.empty()) {
            for (WorkSet::const_iterator f = Q.begin(); f != Q.end(); ++f)
                slicers[*f]->calculateStaticSlice();

            WorkSet tmp;
            for (WorkSet::const_iterator f = Q.begin(); f != Q.end(); ++f) {
                emitToCalls(*f, std::inserter(tmp, tmp.end()));
                emitToExits(*f, std::inserter(tmp, tmp.end()));
            }
            std::swap(tmp,Q);
        }
    }

    bool StaticSlicer::sliceModule() {
      bool modified = false;
      for (Slicers::iterator s = slicers.begin(); s != slicers.end(); ++s)
        modified |= s->second->slice();
      if (modified)
        for (Module::iterator I = module.begin(), E = module.end(); I != E; ++I)
          if (!I->isDeclaration())
            FunctionStaticSlicer::removeUndefs(MP, *I);
      return modified;
    }
}}

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
  };
}

static RegisterPass<Slicer> X("slice-inter", "Slices the code interprocedurally");
char Slicer::ID;

bool Slicer::runOnModule(Module &M) {
  ptr::PointsToSets PS;
  {
    ptr::ProgramStructure P(M);
    computePointsToSets(P, PS);
  }

  callgraph::Callgraph CG(M, PS);

  mods::Modifies MOD;
  {
    mods::ProgramStructure P1(M);
    computeModifies(P1, CG, PS, MOD);
  }

  slicing::StaticSlicer SS(this, M, PS, CG, MOD);
  SS.computeSlice();
  return SS.sliceModule();
}
