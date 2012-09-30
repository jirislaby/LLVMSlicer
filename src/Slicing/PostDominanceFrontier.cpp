//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "llvm/Constants.h"
#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/Pass.h"

#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "PostDominanceFrontier.h"

using namespace llvm;

static RegisterPass<CreateHammockCFG> Y("create-hammock-cfg",
		"Creates Hammock Graph from Control Flow Graph");
char CreateHammockCFG::ID = 0;

bool CreateHammockCFG::runOnFunction(Function &F) {
  if (F.front().getName() == "start")
	  return false;

  LoopInfo &LI = getAnalysis<LoopInfo>();
  LLVMContext &ctx = F.getContext();
  Function::BasicBlockListType &BBL = F.getBasicBlockList();
  ConstantInt *Ctrue = ConstantInt::getTrue(ctx);
  BasicBlock &entry = BBL.front();

  BasicBlock *BBend = BasicBlock::Create(ctx, "end", &F);
  new UnreachableInst(ctx, BBend);

  BasicBlock *BBstart = BasicBlock::Create(ctx, "start", &F, &entry);
  BranchInst::Create(&entry, BBend, Ctrue, BBstart);

  for (Function::iterator I = BBL.begin(), E = BBL.end(); I != E; ++I) {
    BasicBlock *BB = I;
    if (LI.isLoopHeader(BB)) {
      BasicBlock *BBLoopBody = BB->splitBasicBlock(BB->begin(), "body");
      ReplaceInstWithInst(&BB->back(),
		      BranchInst::Create(BBLoopBody, BBend, Ctrue));
    }
    if (BB == BBend)
      continue;
    if (UnreachableInst *UI =
	llvm::dyn_cast<UnreachableInst>(BB->getTerminator())) {
      ReplaceInstWithInst(UI, BranchInst::Create(BBend));
    }
  }

  return true;
}

//===----------------------------------------------------------------------===//
//  PostDominanceFrontier Implementation
//===----------------------------------------------------------------------===//

static RegisterPass<PostDominanceFrontier> X("postdom-frontier", "Computes postdom frontiers");
char PostDominanceFrontier::ID = 0;

#ifdef CONTROL_DEPENDENCE_GRAPH
void PostDominanceFrontier::constructS(const PostDominatorTree &DT,
		Function &F, Stype &S) {
  for (Function::iterator I = F.begin(), E = F.end(); I != E; ++I) {
    BasicBlock *m = I;
    DomTreeNode *mNode = DT[m];
    for (succ_iterator II = succ_begin(m), EE = succ_end(m); II != EE; ++II) {
      BasicBlock *n = *II;
      DomTreeNode *nNode = DT[n];
      if (!DT.properlyDominates(nNode, mNode))
	S.insert(std::make_pair(mNode, nNode));
    }
  }
}

/*
 * Taken from Dominators.h.
 * Changed to return a path from LCA to B and optimized.
 */
const DomTreeNode *
PostDominanceFrontier::findNearestCommonDominator(const PostDominatorTree &DT,
		DomTreeNode *A, DomTreeNode *B) {
  BasicBlock *BB = A->getBlock();
  assert(BB);
  Frontiers[B->getBlock()].insert(BB);

  // If A dominates B then A is nearest common dominator.
  if (DT.dominates(A, B))
    return A;

  /* so the LCA has to be A's parent according to Muchnick */
  DomTreeNode *IDomA = A->getIDom();

  // Walk NodeB immediate dominators chain and find common dominator node.
  DomTreeNode *IDomB = B->getIDom();
  while (IDomB) {
    if (IDomB == IDomA)
      return IDomB;
    Frontiers[IDomB->getBlock()].insert(BB);

    IDomB = IDomB->getIDom();
  }

  assert(0);
  return NULL;
}

void
PostDominanceFrontier::calculate(const PostDominatorTree &DT, Function &F) {
  Stype S;
  constructS(DT, F, S);
  for (Stype::const_iterator I = S.begin(), E = S.end(); I != E; ++I) {
    DomTreeNode *mNode = I->first;
    DomTreeNode *nNode = I->second;
    findNearestCommonDominator(DT, mNode, nNode);
  }
}

#else /* CONTROL_DEPENDENCE_GRAPH */

const DominanceFrontier::DomSetType &
PostDominanceFrontier::calculate(const PostDominatorTree &DT,
                                 const DomTreeNode *Node) {
  // Loop over CFG successors to calculate DFlocal[Node]
  BasicBlock *BB = Node->getBlock();
  DomSetType &S = Frontiers[BB];       // The new set to fill in...
  if (getRoots().empty()) return S;

  if (BB)
    for (pred_iterator SI = pred_begin(BB), SE = pred_end(BB);
         SI != SE; ++SI) {
      BasicBlock *P = *SI;
      // Does Node immediately dominate this predecessor?
      DomTreeNode *SINode = DT[P];
      if (SINode && SINode->getIDom() != Node)
        S.insert(P);
    }

  // At this point, S is DFlocal.  Now we union in DFup's of our children...
  // Loop through and visit the nodes that Node immediately dominates (Node's
  // children in the IDomTree)
  //
  for (DomTreeNode::const_iterator
         NI = Node->begin(), NE = Node->end(); NI != NE; ++NI) {
    DomTreeNode *IDominee = *NI;
    const DomSetType &ChildDF = calculate(DT, IDominee);

    DomSetType::const_iterator CDFI = ChildDF.begin(), CDFE = ChildDF.end();
    for (; CDFI != CDFE; ++CDFI) {
      if (!DT.properlyDominates(Node, DT[*CDFI]))
        S.insert(*CDFI);
    }
  }

  return S;
}
#endif
