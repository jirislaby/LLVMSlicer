//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POST_DOMINANCE_FRONTIER
#define POST_DOMINANCE_FRONTIER

#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/PostDominators.h"

namespace llvm {
  /// PostDominanceFrontier Class - Concrete subclass of DominanceFrontier that is
  /// used to compute the a post-dominance frontier.
  ///
  struct PostDominanceFrontier : public DominanceFrontierBase {
    static char ID;
    PostDominanceFrontier()
      : DominanceFrontierBase(ID, true) { }

    virtual bool runOnFunction(Function &) {
      Frontiers.clear();
      PostDominatorTree &DT = getAnalysis<PostDominatorTree>();
      Roots = DT.getRoots();
      if (const DomTreeNode *Root = DT.getRootNode())
        calculate(DT, Root);
      return false;
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      AU.addRequired<PostDominatorTree>();
    }

  private:
    const DomSetType &calculate(const PostDominatorTree &DT,
                                const DomTreeNode *Node);
  };
}

#endif
