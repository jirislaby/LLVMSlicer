// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/Pass.h"
#include "llvm/IR/Value.h"

#include "FunctionStaticSlicer.h"
#include "../Callgraph/Callgraph.h"
#include "../Modifies/Modifies.h"
#include "../PointsTo/PointsTo.h"

using namespace llvm;

namespace llvm { namespace slicing { namespace detail {

    typedef ptr::PointsToSets::Pointee Pointee;
    typedef std::map<const Pointee, const Pointee> ParamsToArgs;
    typedef std::set<Pointee> RelevantSet;

    static void fillParamsToArgs(const CallInst *C, const Function *F,
			  ParamsToArgs &toArgs)
    {
	Function::const_arg_iterator p = F->arg_begin();

	for (unsigned a = 0; a < C->getNumArgOperands(); ++a, ++p) {
	    const Value *P = &*p;
	    const Value *A = C->getArgOperand(a);
	    if (!isConstantValue(A))
		toArgs.insert(ParamsToArgs::value_type(Pointee(P, -1),
					Pointee(A, -1)));
	}
    }

    static void getRelevantVarsAtCall(const CallInst *C, const Function *F,
			       ValSet::const_iterator b,
			       const ValSet::const_iterator &e,
			       RelevantSet &out) {
	assert(!isInlineAssembly(C) && "Inline assembly is not supported!");

	ParamsToArgs toArgs;
	fillParamsToArgs(C, F, toArgs);

	for (; b != e; ++b) {
	    ParamsToArgs::const_iterator it = toArgs.find(*b);
	    if (it != toArgs.end())
		out.insert(it->second);
	    else if (!isLocalToFunction(b->first, F))
		out.insert(*b);
	}
    }

    static void getRelevantVarsAtExit(const CallInst *C, const ReturnInst *R,
			       ValSet::const_iterator b,
			       const ValSet::const_iterator &e,
			       RelevantSet &out) {
	assert(!isInlineAssembly(C) && "Inline assembly is not supported!");

	if (callToVoidFunction(C)) {
	    std::copy(b, e, std::inserter(out, out.begin()));
	    return;
	}

	for ( ; b != e; ++b)
	    if (b->first == C) {
		Value *ret = R->getReturnValue();
		if (!ret) {
/*		    C->dump();
		    C->getCalledValue()->dump();
		    R->dump();*/
//		    abort();
		    return;
		}
		out.insert(Pointee(R->getReturnValue(), -1));
	    } else
		out.insert(*b);
    }

}}}

namespace llvm { namespace slicing {

    class StaticSlicer {
    public:
        typedef std::map<llvm::Function const*, FunctionStaticSlicer *> Slicers;
        typedef std::multimap<llvm::Function const*,llvm::CallInst const*>
                FuncsToCalls;
        typedef std::multimap<llvm::CallInst const*,llvm::Function const*>
                CallsToFuncs;

        StaticSlicer(ModulePass *MP, Module &M,
		     const ptr::PointsToSets &PS,
                     const callgraph::Callgraph &CG,
                     const mods::Modifies &MOD);

        ~StaticSlicer();

        void computeSlice();
        bool sliceModule();

    private:
        typedef llvm::SmallVector<const llvm::Function *, 20> InitFuns;

	void buildDicts(const ptr::PointsToSets &PS, const CallInst *c);
        void buildDicts(const ptr::PointsToSets &PS);

        template<typename OutIterator>
        void emitToCalls(llvm::Function const* const f, OutIterator out);

        template<typename OutIterator>
        void emitToExits(llvm::Function const* const f, OutIterator out);

        void runFSS(Function &F, const ptr::PointsToSets &PS,
                    const callgraph::Callgraph &CG, const mods::Modifies &MOD);

        ModulePass *MP;
        Module &module;
        Slicers slicers;
        InitFuns initFuns;
        FuncsToCalls funcsToCalls;
        CallsToFuncs callsToFuncs;
    };

    template<typename OutIterator>
    void StaticSlicer::emitToCalls(const Function *f, OutIterator out) {
	const Instruction *entry = getFunctionEntry(f);
	const ValSet::const_iterator relBgn = slicers[f]->relevant_begin(entry);
        const ValSet::const_iterator relEnd = slicers[f]->relevant_end(entry);

        FuncsToCalls::const_iterator c, e;
        llvm::tie(c, e) = funcsToCalls.equal_range(f);

        for ( ; c != e; ++c) {
	    const CallInst *CI = c->second;
	    const Function *g = CI->getParent()->getParent();
	    FunctionStaticSlicer *FSS = slicers[g];

	    detail::RelevantSet R;
	    detail::getRelevantVarsAtCall(c->second, f, relBgn, relEnd, R);

	    if (FSS->addCriterion(CI, R.begin(), R.end(),
				    !FSS->shouldSkipAssert(CI))) {
		FSS->addCriterion(CI, FSS->REF_begin(CI), FSS->REF_end(CI));
                *out++ = g;
	    }
        }
    }

    template<typename OutIterator>
    void StaticSlicer::emitToExits(const Function *f, OutIterator out) {
        typedef std::vector<const CallInst *> CallsVec;

        CallsVec C;
        getFunctionCalls(f, std::back_inserter(C));

        for (CallsVec::const_iterator c = C.begin(); c != C.end(); ++c) {
	    const ValSet::const_iterator relBgn =
                slicers[f]->relevant_begin(getSuccInBlock(*c));
            const ValSet::const_iterator relEnd =
                slicers[f]->relevant_end(getSuccInBlock(*c));

            CallsToFuncs::const_iterator g, e;
            llvm::tie(g, e) = callsToFuncs.equal_range(*c);

            for ( ; g != e; ++g) {
                typedef std::vector<const llvm::ReturnInst *> ExitsVec;
		const Function *callie = g->second;

                ExitsVec E;
                getFunctionExits(callie, std::back_inserter(E));

                for (ExitsVec::const_iterator e = E.begin(); e != E.end(); ++e) {
		    detail::RelevantSet R;
		    detail::getRelevantVarsAtExit(*c, *e, relBgn, relEnd, R);
                    if (slicers[g->second]->addCriterion(*e, R.begin(),R .end()))
                        *out++ = g->second;
                }
            }
        }
    }

    void StaticSlicer::buildDicts(const ptr::PointsToSets &PS,
		const CallInst *c) {
	typedef std::vector<const Function *> FunCon;
	FunCon G;
	getCalledFunctions(c, PS, std::back_inserter(G));

	for (FunCon::const_iterator I = G.begin(), E = G.end(); I != E; ++I) {
	    const Function *h = *I;

	    if (!memoryManStuff(h) && !h->isDeclaration()) {
		funcsToCalls.insert(std::make_pair(h, c));
		callsToFuncs.insert(std::make_pair(c, h));
	    }
	}
    }

    void StaticSlicer::buildDicts(const ptr::PointsToSets &PS)
    {
        for (Module::const_iterator f = module.begin(); f != module.end(); ++f)
            if (!f->isDeclaration() && !memoryManStuff(&*f))
                for (const_inst_iterator I = inst_begin(*f), E = inst_end(*f);
			I != E; ++I)
                    if (const CallInst *c = dyn_cast<CallInst>(&*I)) {
			if (isInlineAssembly(c)) {
			    errs() << "ERROR: Inline assembler detected in " <<
				f->getName() << ", skipping\n";
			    continue;
			}

			buildDicts(PS, c);
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
