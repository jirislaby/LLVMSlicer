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
#include "../Callgraph/Callgraph.h"
#include "../PointsTo/PointsTo.h"
#include "../Languages/LLVM.h"
#include "../Languages/LLVMSupport.h"

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


}}

namespace llvm { namespace slicing { namespace detail {

    typedef std::map<const Pointee, const Pointee> ParamsToArgs;

    void fillParamsToArgs(llvm::CallInst const* const C,
                          llvm::Function const* const F,
                          ParamsToArgs& toArgs);

    typedef std::set<Pointee> RelevantSet;

    void getRelevantVarsAtCall(llvm::CallInst const* const C,
                               llvm::Function const* const F,
			       const ValSet::const_iterator &b,
			       const ValSet::const_iterator &e,
			       RelevantSet &out);

    void getRelevantVarsAtExit(llvm::CallInst const* const C,
                               llvm::ReturnInst const* const R,
			       ValSet::const_iterator &b,
			       const ValSet::const_iterator &e,
			       RelevantSet &out);

}}}

namespace llvm { namespace slicing {

    template<typename OutIterator>
    void StaticSlicer::emitToCalls(llvm::Function const* const f,
                                   OutIterator out) {
	const ValSet::const_iterator relBgn =
            slicers[f]->relevant_begin(getFunctionEntry(f));
        const ValSet::const_iterator relEnd =
            slicers[f]->relevant_end(getFunctionEntry(f));
        FuncsToCalls::const_iterator c, e;
        llvm::tie(c,e) = funcsToCalls.equal_range(f);
        for ( ; c != e; ++c) {
	    const llvm::CallInst *CI = c->second;
	    const llvm::Function *g = CI->getParent()->getParent();
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
    void StaticSlicer::emitToExits(llvm::Function const* const f,
                                   OutIterator out) {
        typedef std::vector<const llvm::CallInst *> CallsVec;
        CallsVec C;
        getFunctionCalls(f, std::back_inserter(C));
        for (CallsVec::const_iterator c = C.begin(); c != C.end(); ++c) {
	    ValSet::const_iterator relBgn =
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

}}

#endif
