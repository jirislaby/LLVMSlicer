//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POST_DOMINANCE_FRONTIER
#define POST_DOMINANCE_FRONTIER

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/PostDominators.h"

namespace llvm {

  struct CreateHammockCFG : public FunctionPass {
    static char ID;

    CreateHammockCFG() : FunctionPass(ID) { }

    virtual bool runOnFunction(Function &F);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<LoopInfo>();
    }
  };

  /// PostDominanceFrontier Class - Concrete subclass of DominanceFrontier that is
  /// used to compute the a post-dominance frontier.
  ///
  struct PostDominanceFrontier : public DominanceFrontierBase {
    static char ID;
    PostDominanceFrontier()
      : DominanceFrontierBase(ID, true) { }

    virtual bool runOnFunction(Function &F) {
      Frontiers.clear();
      PostDominatorTree &DT = getAnalysis<PostDominatorTree>();
#ifdef CONTROL_DEPENDENCE_GRAPH
      calculate(DT, F);
#else
      Roots = DT.getRoots();
      if (const DomTreeNode *Root = DT.getRootNode()) {
        calculate(DT, Root);
#ifdef PDF_DUMP
	errs() << "=== DUMP:\n";
	dump();
	errs() << "=== EOD\n";
#endif
      }
#endif
      return false;
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      AU.addRequired<PostDominatorTree>();
    }

  private:
#ifdef CONTROL_DEPENDENCE_GRAPH
    typedef std::pair<DomTreeNode *, DomTreeNode *> Ssubtype;
    typedef std::set<Ssubtype> Stype;

    void calculate(const PostDominatorTree &DT, Function &F);
    void constructS(const PostDominatorTree &DT, Function &F, Stype &S);
    const DomTreeNode *findNearestCommonDominator(const PostDominatorTree &DT,
		    DomTreeNode *A, DomTreeNode *B);
#else
    const DomSetType &calculate(const PostDominatorTree &DT,
                                const DomTreeNode *Node);
#endif
  };
}

#endif
