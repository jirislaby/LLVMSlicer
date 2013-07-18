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

    void getRelevantVarsAtCall(llvm::CallInst const* const C,
			       llvm::Function const* const F,
			       const ValSet::const_iterator &_b,
			       const ValSet::const_iterator &e,
			       RelevantSet &out) {
	assert(!isInlineAssembly(C) && "Inline assembly is not supported!");

	ParamsToArgs toArgs;
	fillParamsToArgs(C, F, toArgs);

	for (ValSet::const_iterator b(_b); b != e; ++b) {
	    ParamsToArgs::const_iterator it = toArgs.find(*b);
	    if (it != toArgs.end())
		out.insert(it->second);
	    else if (!isLocalToFunction(*b,F))
		out.insert(*b);
	}
    }

    void getRelevantVarsAtExit(const llvm::CallInst *const C,
			       const llvm::ReturnInst *const R,
			       ValSet::const_iterator &b,
			       const ValSet::const_iterator &e,
			       RelevantSet &out) {
	assert(!isInlineAssembly(C) && "Inline assembly is not supported!");

	if (callToVoidFunction(C)) {
	    std::copy(b, e, std::inserter(out, out.begin()));
	    return;
	}

	for ( ; b != e; ++b)
	    if (*b == C) {
		    Value *ret = R->getReturnValue();
		    if (!ret) {
/*			    C->dump();
			    C->getCalledValue()->dump();
			    R->dump();*/
//			    abort();
				return;
		    }
		out.insert(R->getReturnValue());
	    } else
		out.insert(*b);
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

    StaticSlicer::StaticSlicer(ModulePass *MP, Module &M,
                               const ptr::PointsToSets &PS,
                               const callgraph::Callgraph &CG,
                               const mods::Modifies &MOD) : MP(MP), module(M),
                               slicers(), initFuns(), funcsToCalls(),
                               callsToFuncs() {
        for (Module::iterator f = M.begin(); f != M.end(); ++f)
          if (!f->isDeclaration() && !memoryManStuff(&*f))
            runFSS(*f, PS, CG, MOD);
        buildDicts(PS);
    }

    StaticSlicer::~StaticSlicer() {
      for (Slicers::const_iterator I = slicers.begin(), E = slicers.end();
           I != E; ++I)
        delete I->second;
    }

    void StaticSlicer::runFSS(Function &F, const ptr::PointsToSets &PS,
			      const callgraph::Callgraph &CG,
			      const mods::Modifies &MOD) {
      callgraph::Callgraph::range_iterator callees = CG.callees(&F);
      bool starting = std::distance(callees.first, callees.second) == 0;

      FunctionStaticSlicer *FSS = new FunctionStaticSlicer(F, MP, PS, MOD);
      bool hadAssert = slicing::findInitialCriterion(F, *FSS, starting);

      /*
       * Functions with an assert might not have a return and slicer wouldn't
       * compute them at all in that case.
       */
      if (starting || hadAssert)
	initFuns.push_back(&F);

      slicers.insert(Slicers::value_type(&F, FSS));
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
