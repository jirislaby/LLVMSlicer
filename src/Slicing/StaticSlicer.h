#ifndef SLICING_STATICSLICER_H
#define SLICING_STATICSLICER_H

#include <map>
#include <set>
#include <vector>
#include <iterator>
#include <algorithm>

#include "llvm/ADT/STLExtras.h" /* tie */
#include "llvm/Analysis/PostDominators.h"

#include "FunctionStaticSlicer.h"
#include "../PointsTo/PointsTo.h"
#include "../Languages/LLVM.h"

namespace llvm { namespace slicing {

    class StaticSlicer {
    public:
        typedef std::map<llvm::Function const*, FunctionStaticSlicer *> Slicers;
        typedef std::multimap<llvm::Function const*,llvm::CallInst const*>
                FuncsToCalls;
        typedef std::multimap<llvm::CallInst const*,llvm::Function const*>
                CallsToFuncs;

        template<typename PointsToSets, typename ModifiesSets>
        StaticSlicer(ModulePass *MP, Module &M, const PointsToSets &PS,
                     const ModifiesSets &MOD);

        ~StaticSlicer();

	template<typename FwdInstrIterator, typename FwdValueIterator>
	void computeSlice(FwdInstrIterator ib, const FwdInstrIterator ie,
			  FwdValueIterator vb, const FwdValueIterator ve);

        template<typename FwdValueIterator>
	void computeSlice(const llvm::Instruction *I, FwdValueIterator b,
			  const FwdValueIterator e) {
	    computeSlice(&I, &I + 1, b, e);
	}

        void computeSlice(llvm::Instruction* const I, const llvm::Value *V) {
	    computeSlice(I, &V, &V + 1);
	}

        bool sliceModule();

    private:
        template<typename PointsToSets>
        void buildDicts(PointsToSets const& PS);

        template<typename OutIterator>
        void emitToCalls(llvm::Function const* const f, OutIterator out);

        template<typename OutIterator>
        void emitToExits(llvm::Function const* const f, OutIterator out);

        template<typename PointsToSets, typename ModifiesSets>
        void runFSS(Function &F, const PointsToSets &PS,
                    const ModifiesSets &MOD);

        ModulePass *MP;
        Module &module;
        Slicers slicers;
        FuncsToCalls funcsToCalls;
        CallsToFuncs callsToFuncs;
    };


}}

namespace llvm { namespace slicing { namespace detail {

    typedef std::map<llvm::Value const*,llvm::Value const*>
            ParamsToArgs;

    void fillParamsToArgs(llvm::CallInst const* const C,
                          llvm::Function const* const F,
                          ParamsToArgs& toArgs);

    template<typename RelevantsIterator, typename OutIterator>
    void getRelevantVarsAtCall(llvm::CallInst const* const C,
                               llvm::Function const* const F,
                               RelevantsIterator b, RelevantsIterator const e,
                               //PointsToSets const& PS,
                               OutIterator out) {
	assert(!isInlineAssembly(C) || "Inline assembly is not supported!");
        ParamsToArgs toArgs;
        fillParamsToArgs(C,F,toArgs);
        for ( ; b != e; ++b)
        {
            ParamsToArgs::const_iterator it = toArgs.find(*b);
            if (it != toArgs.end())
                *out++ = it->second;
            else if (!isLocalToFunction(*b,F))
                *out++ = *b;
        }
    }

    template<typename RelevantsIterator, typename OutIterator>
    void getRelevantVarsAtExit(llvm::CallInst const* const C,
                               llvm::ReturnInst const* const R,
                               RelevantsIterator b, RelevantsIterator const e,
                               OutIterator out) {
	assert(!isInlineAssembly(C) || "Inline assembly is not supported!");
	if (callToVoidFunction(C))
        {
            std::copy(b,e,out);
            return;
        }
        for ( ; b != e; ++b)
            if (*b == C)
                *out++ = R->getOperand(0);
            else
                *out++ = *b;
    }

}}}

namespace llvm { namespace slicing {

    template<typename PointsToSets, typename ModifiesSets>
    StaticSlicer::StaticSlicer(ModulePass *MP, Module &M,
                               PointsToSets const& PS,
                               ModifiesSets const& MOD) : MP(MP), module(M),
                               slicers(), funcsToCalls(), callsToFuncs() {
        for (llvm::Module::iterator f = M.begin(); f != M.end(); ++f)
          if (!f->isDeclaration() && !memoryManStuff(f))
            runFSS(*f, PS, MOD);
        buildDicts(PS);
    }

    StaticSlicer::~StaticSlicer() {
      for (Slicers::const_iterator I = slicers.begin(), E = slicers.end();
           I != E; ++I)
        delete I->second;
    }

  template<typename PointsToSets, typename ModifiesSets>
  void StaticSlicer::runFSS(Function &F, const PointsToSets &PS,
                            const ModifiesSets &MOD) {
    FunctionStaticSlicer *FSS = new FunctionStaticSlicer(F, MP, PS, MOD);
    llvm::slicing::findInitialCriterion(F, *FSS);
    slicers.insert(Slicers::value_type(&F, FSS));
  }

    template<typename PointsToSets>
    void StaticSlicer::buildDicts(PointsToSets const& PS)
    {
        typedef llvm::Module::iterator FunctionsIter;
        for (FunctionsIter f = module.begin(); f != module.end(); ++f)
            if (!f->isDeclaration() && !memoryManStuff(f))
                for (llvm::inst_iterator i = llvm::inst_begin(*f);
                        i != llvm::inst_end(*f); i++)
                    if (llvm::CallInst const* c =
                            llvm::dyn_cast<llvm::CallInst const>(&*i))
                    {
			if (isInlineAssembly(c)) {
			    errs() << "ERROR: Inline assembler detected\n";
			    continue;
			}
                        std::vector<llvm::Value const*> G;
                        if (c->getCalledFunction() != 0)
                            G.push_back(c->getCalledFunction());
                        else
                        {
                            typename PointsToSets::PointsToSet const& S =
                                getPointsToSet(c->getCalledValue(),PS);
                            std::copy(S.begin(),S.end(),std::back_inserter(G));
                        }
                        for (std::vector<llvm::Value const*>::const_iterator g =
                                G.begin(); g != G.end(); ++g)
                        {
                            llvm::Function const* const h =
                                llvm::dyn_cast<llvm::Function>(*g);
                            if (!memoryManStuff(h) && !h->isDeclaration())
                            {
                                funcsToCalls.insert(std::make_pair(h,c));
                                callsToFuncs.insert(std::make_pair(c,h));
                            }
                        }
                    }
    }

    template<typename OutIterator>
    void StaticSlicer::emitToCalls(llvm::Function const* const f,
                                   OutIterator out) {
	ValSet::const_iterator const relBgn =
            slicers[f]->relevant_begin(getFunctionEntry(f));
        ValSet::const_iterator const relEnd =
            slicers[f]->relevant_end(getFunctionEntry(f));
        FuncsToCalls::const_iterator c,e;
        llvm::tie(c,e) = funcsToCalls.equal_range(f);
        for ( ; c != e; ++c)
        {
            const llvm::Function *g = c->second->getParent()->getParent();
            std::set<const llvm::Value *> R;
            detail::getRelevantVarsAtCall(c->second,f,relBgn,relEnd,/*PS,*/
                                          std::inserter(R,R.end()));
            if (slicers[g]->addCriterion(c->second,R.begin(),R.end()))
                *out++ = g;
        }
    }

    template<typename OutIterator>
    void StaticSlicer::emitToExits(llvm::Function const* const f,
                                   OutIterator out)
    {
        typedef std::vector<llvm::CallInst const*> CallsVec;
        CallsVec C;
        getFunctionCalls(f,std::back_inserter(C));
        for (CallsVec::const_iterator c = C.begin(); c != C.end(); ++c)
        {
            ValSet::const_iterator const relBgn =
                slicers[f]->relevant_begin(getSuccInBlock(*c));
            ValSet::const_iterator const relEnd =
                slicers[f]->relevant_end(getSuccInBlock(*c));
            CallsToFuncs::const_iterator g,e;
            llvm::tie(g,e) = callsToFuncs.equal_range(*c);
            for ( ; g != e; ++g)
            {
                typedef std::vector<llvm::ReturnInst const*> ExitsVec;
                ExitsVec E;
                getFunctionExits(g->second,std::back_inserter(E));
                for (ExitsVec::const_iterator e = E.begin(); e != E.end(); ++e)
                {
                    std::set<llvm::Value const*> R;
                    detail::getRelevantVarsAtExit(*c,*e,relBgn,relEnd,
                                                  std::inserter(R,R.end()));
                    if (slicers[g->second]->addCriterion(*e,R.begin(),R.end()))
                        *out++ = g->second;
                }
            }
        }
    }

    template<typename FwdInstrIterator, typename FwdValueIterator>
    void StaticSlicer::computeSlice(FwdInstrIterator ib,
				    const FwdInstrIterator ie,
                                    FwdValueIterator vb,
				    const FwdValueIterator ve) {
        typedef std::set<llvm::Function const*> WorkSet;
        WorkSet Q;
	for ( ; ib != ie; ++ib) {
	    llvm::Function const* const startFn = (*ib)->getParent()->getParent();
	    slicers[startFn]->addCriterion(*ib,vb,ve,true);
	    Q.insert(startFn);
	}
	while (!Q.empty()) {
            for (WorkSet::const_iterator f = Q.begin(); f != Q.end(); ++f)
                slicers[*f]->calculateStaticSlice();

            WorkSet tmp;
            for (WorkSet::const_iterator f = Q.begin(); f != Q.end(); ++f)
            {
                emitToCalls(*f,std::inserter(tmp,tmp.end()));
                emitToExits(*f,std::inserter(tmp,tmp.end()));
            }
            using std::swap;
            swap(tmp,Q);
        }
    }

}}

#if 0
inline std::ostream&
operator<<(std::ostream& ostr, monty::codespy::slicing::StaticSlicer const& S)
{
    S.dump(ostr);
    return ostr;
}
#endif

#endif
