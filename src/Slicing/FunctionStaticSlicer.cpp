//===- Hello.cpp - Example code from "Writing an LLVM Pass" ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// Details are in a white paper by F. Tip called:
// A survey of program slicing techniques
//===----------------------------------------------------------------------===//

#include <ctype.h>
#include <map>

#include "llvm/Constants.h"
#include "llvm/Function.h"
#include "llvm/GlobalVariable.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TypeBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "PostDominanceFrontier.h"
#include "../Callgraph/Callgraph.h"
#include "../Modifies/Modifies.h"
#include "../Modifies/AlgoDumbSpeedy.h"
#include "../PointsTo/AlgoAndersen.h"
#include "../PointsTo/PointsTo.h"

#include "FunctionStaticSlicer.h"
#include "Prepare.h"

using namespace llvm;
using namespace llvm::slicing;

template<typename PointsToSets, typename ModifiesSets>
InsInfo::InsInfo(const Instruction *i, PointsToSets const& PS,
                 ModifiesSets const& MOD) : ins(i), sliced(true) {
  if (const LoadInst *LI = dyn_cast<const LoadInst>(i)) {
    DEF.insert(i);

    const Value *op = LI->getPointerOperand();
    REF.insert(op);
    if (!hasExtraReference(op)) {
      typename PointsToSets::PointsToSet const& S = getPointsToSet(op, PS);
      for (typename PointsToSets::PointsToSet::const_iterator p = S.begin();
           p != S.end(); ++p)
        REF.insert(*p);
    }
  } else if (const StoreInst *SI = dyn_cast<const StoreInst>(i)) {
    const Value *l = SI->getPointerOperand();
    if (hasExtraReference(l)) {
      DEF.insert(l);
    } else {
      typename PointsToSets::PointsToSet const& S = getPointsToSet(l, PS);
      for (typename PointsToSets::PointsToSet::const_iterator p = S.begin();
           p != S.end(); ++p)
	      DEF.insert(*p);
    }

    const Value *r = SI->getValueOperand();
    if (!r->getType()->isIntegerTy())
      REF.insert(l);
    if (!hasExtraReference(r) && !llvm::isa<llvm::Constant>(r))
      REF.insert(r);
  } else if (const GetElementPtrInst *gep =
             dyn_cast<const GetElementPtrInst>(i)) {
    DEF.insert(i);

    REF.insert(gep->getPointerOperand());
  } else if (CallInst const* const C = dyn_cast<const CallInst>(i)) {
    const Value *cv = C->getCalledValue();

    if (isInlineAssembly(C)) {
     errs() << "ERROR: Inline assembler detected in " <<
          i->getParent()->getParent()->getName() << ", ignoring\n";
    } else {
#if 0 /* bullshit, we do not have malloc/free and other crap */
    } else if (isMemoryAllocation(cv)) {
      DEF.insert(i);
    } else if (isMemoryDeallocation(cv)) {
    } else if (isMemoryCopy(cv) || isMemoryMove(cv)) {
      llvm::Value const* const l = C->getOperand(0);
      PointsToSets::PointsToSet const& L = getPointsToSet(l,PS);
      for (PointsToSets::PointsToSet::const_iterator
        p = L.begin(); p != L.end(); ++p)
          DEF.insert(*p);

      llvm::Value const* const r = C->getOperand(1);
      REF.insert(l);
      REF.insert(r);
      PointsToSets::PointsToSet const& R = getPointsToSet(r,PS);
      for (PointsToSets::PointsToSet::const_iterator p = R.begin();
          p != R.end(); ++p)
        REF.insert(*p);
    } else if (!memoryManStuff(C)) {
#endif
      typedef std::vector<llvm::Function const*> CalledVec;
      CalledVec CV;
      if (C->getCalledFunction() != 0) {
          CV.push_back(C->getCalledFunction());
      } else {
        typename PointsToSets::PointsToSet const& R = getPointsToSet(cv,PS);
        for (typename PointsToSets::PointsToSet::const_iterator p = R.begin();
             p != R.end(); ++p)
          if (const Function *fn = dyn_cast<const Function>(*p))
            CV.push_back(fn);
      }

      for (CalledVec::const_iterator f = CV.begin(); f != CV.end(); ++f) {
        typename ModifiesSets::mapped_type const& M = getModSet(*f, MOD);
        for (typename ModifiesSets::mapped_type::const_iterator v = M.begin();
             v != M.end(); ++v)
          DEF.insert(*v);
      }

      if (!callToVoidFunction(C))
          DEF.insert(C);
    }
  } else if (isa<const ReturnInst>(i)) {
  } else if (const BinaryOperator *BO = dyn_cast<const BinaryOperator>(i)) {
    DEF.insert(i);

    if (!llvm::isa<llvm::Constant>(BO->getOperand(0)))
      REF.insert(BO->getOperand(0));
    if (!llvm::isa<llvm::Constant>(BO->getOperand(1)))
      REF.insert(BO->getOperand(1));
  } else if (const CastInst *CI = dyn_cast<const CastInst>(i)) {
    DEF.insert(i);

    if (!hasExtraReference(CI->getOperand(0)))
      REF.insert(CI->getOperand(0));
  } else if (const AllocaInst *AI = dyn_cast<const AllocaInst>(i)) {
      DEF.insert(AI);

      REF.insert(AI);
  } else if (const CmpInst *CI = dyn_cast<const CmpInst>(i)) {
    DEF.insert(i);

    if (!llvm::isa<llvm::Constant>(CI->getOperand(0)))
      REF.insert(CI->getOperand(0));
    if (!llvm::isa<llvm::Constant>(CI->getOperand(1)))
      REF.insert(CI->getOperand(1));
  } else if (const BranchInst *BI = dyn_cast<const BranchInst>(i)) {
    if (BI->isConditional())
      REF.insert(BI->getCondition());
  } else if (const PHINode *phi = dyn_cast<const PHINode>(i)) {
    DEF.insert(i);

    for (unsigned k = 0; k < phi->getNumIncomingValues(); ++k)
      if (!isa<Constant>(phi->getIncomingValue(k)))
	REF.insert(phi->getIncomingValue(k));
  } else if (isa<const SwitchInst>(i)) {
  } else if (const SelectInst *SI = dyn_cast<const SelectInst>(i)) {
      // TODO: THE FOLLOWING CODE HAS NOT BEEN TESTED YET

    DEF.insert(i);

    if (!isa<Constant>(SI->getCondition()))
      REF.insert(SI->getCondition());
    if (!isa<Constant>(SI->getTrueValue()))
      REF.insert(SI->getTrueValue());
    if (!isa<Constant>(SI->getFalseValue()))
      REF.insert(SI->getFalseValue());
  } else if (isa<const UnreachableInst>(i)) {
  } else if (const ExtractValueInst *EV = dyn_cast<const ExtractValueInst>(i)) {
      DEF.insert(i);
      REF.insert(EV->getAggregateOperand());
  } else if (const InsertValueInst *IV = dyn_cast<const InsertValueInst>(i)) {
//      TODO THE FOLLOWING CODE HAS NOT BEEN TESTED YET

      DEF.insert(IV->getAggregateOperand());
      const Value *r = IV->getInsertedValueOperand();
      if (!isa<Constant>(r))
	REF.insert(r);
  } else {
    errs() << "ERROR: Unsupported instruction reached\n";
    i->print(errs());
  }
}

namespace {
  class FunctionSlicer : public ModulePass {
    public:
      static char ID;

      FunctionSlicer() : ModulePass(ID) {}

      virtual bool runOnModule(Module &M);

      void getAnalysisUsage(AnalysisUsage &AU) const {
        AU.addRequired<PostDominatorTree>();
        AU.addRequired<PostDominanceFrontier>();
      }
    private:
      template<typename PointsToSets, typename ModifiesSets>
      bool runOnFunction(Function &F, const PointsToSets &PS,
                         const ModifiesSets &MOD);
  };
}

static RegisterPass<FunctionSlicer> X("slice", "Slices the code");
char FunctionSlicer::ID;

FunctionStaticSlicer::~FunctionStaticSlicer() {
  for (InsInfoMap::const_iterator I = insInfoMap.begin(), E = insInfoMap.end();
       I != E; I++)
    delete I->second;
}

typedef llvm::SmallVector<const Instruction *, 10> SuccList;

static SuccList getSuccList(const Instruction *i) {
  SuccList succList;
  const BasicBlock *bb = i->getParent();
  if (i != &bb->back()) {
    BasicBlock::const_iterator I(i);
    I++;
    succList.push_back(&*I);
  } else {
    for (succ_const_iterator I = succ_begin(bb), E = succ_end(bb); I != E; I++)
      succList.push_back(&(*I)->front());
  }
  return succList;
}

/*
 * RC(i)=RC(i) \cup
 *   {v| v \in RC(j), v \notin DEF(i)} \cup
 *   {v| v \in REF(i), DEF(i) \cap RC(j) \neq \emptyset}
 */
bool FunctionStaticSlicer::computeRCi(InsInfo *insInfoi, InsInfo *insInfoj) {
  bool changed = false;

  /* {v| v \in RC(j), v \notin DEF(i)} */
  for (ValSet::const_iterator I = insInfoj->RC_begin(),
       E = insInfoj->RC_end(); I != E; I++) {
    const Value *RCj = *I;
    bool in_DEF = false;
    for (ValSet::const_iterator II = insInfoi->DEF_begin(),
         EE = insInfoi->DEF_end(); II != EE; II++)
      if (*II == RCj) {
        in_DEF = true;
        break;
      }
    if (!in_DEF)
      if (insInfoi->addRC(RCj))
        changed = true;
  }
  /* DEF(i) \cap RC(j) \neq \emptyset */
  bool isect_nonempty = false;
  for (ValSet::const_iterator I = insInfoi->DEF_begin(),
       E = insInfoi->DEF_end(); I != E && !isect_nonempty; I++) {
    const Value *DEFi = *I;
    for (ValSet::const_iterator II = insInfoj->RC_begin(),
         EE = insInfoj->RC_end(); II != EE; II++) {
      if (DEFi == *II) {
        isect_nonempty = true;
        break;
      }
    }
  }

  /* {v| v \in REF(i), ...} */
  if (isect_nonempty)
    for (ValSet::const_iterator I = insInfoi->REF_begin(),
         E = insInfoi->REF_end(); I != E; I++)
      if (insInfoi->addRC(*I))
        changed = true;
#ifdef DEBUG_RC
  errs() << "  " << __func__ << "2 END";
  if (changed)
    errs() << " ----------CHANGED";
  errs() << '\n';
#endif
  return changed;
}

bool FunctionStaticSlicer::computeRCi(const Instruction *i) {
  bool changed = false;
#ifdef DEBUG_RC
  errs() << "  " << __func__ << ": " << i->getOpcodeName();
  if (i->hasName())
    errs() << " (" << i->getNameStr() << ")";
  errs() << '\n';
  errs() << "    DUMP: ";
  i->print(errs());
  errs() << '\n';
#endif
  SuccList succList = getSuccList(i);
  InsInfo *insInfoi = getInsInfo(i);
  for (SuccList::const_iterator I = succList.begin(), E = succList.end();
       I != E; I++)
    changed |= computeRCi(insInfoi, getInsInfo(*I));

  return changed;
}

void FunctionStaticSlicer::computeRC() {
  bool changed;
#ifdef DEBUG_RC
  int it = 1;
#endif
  do {
    changed = false;
#ifdef DEBUG_RC
    errs() << __func__ << ": ============== Iteration " << it++ << '\n';
#endif
    for (inst_iterator I = inst_begin(fun), E = inst_end(fun); I != E; I++)
        changed |= computeRCi(&*I);
  } while (changed);
}

/*
 * SC(i)={i| DEF(i) \cap RC(j) \neq \emptyset}
 */
void FunctionStaticSlicer::computeSCi(const Instruction *i, const Instruction *j) {
  InsInfo *insInfoi = getInsInfo(i), *insInfoj = getInsInfo(j);

  bool isect_nonempty = false;
  for (ValSet::const_iterator I = insInfoi->DEF_begin(),
       E = insInfoi->DEF_end(); I != E && !isect_nonempty; I++) {
    const Value *DEFi = *I;
    for (ValSet::const_iterator II = insInfoj->RC_begin(),
         EE = insInfoj->RC_end(); II != EE; II++) {
      if (DEFi == *II) {
        isect_nonempty = true;
        break;
      }
    }
  }

  if (isect_nonempty) {
    insInfoi->deslice();
#ifdef DEBUG_SLICING
    errs() << "XXXXXXXXXXXXXY ";
    i->print(errs());
    errs() << '\n';
#endif
  }
}

void FunctionStaticSlicer::computeSC() {
  for (inst_iterator I = inst_begin(fun), E = inst_end(fun); I != E; I++) {
    const Instruction *i = &*I;
    SuccList succList = getSuccList(i);
    for (SuccList::const_iterator II = succList.begin(), EE = succList.end();
         II != EE; II++)
      computeSCi(i, *II);
  }
}

bool FunctionStaticSlicer::computeBC() {
  bool changed = false;
#ifdef DEBUG_BC
  errs() << __func__ << " ============ BEG\n";
#endif
  for (inst_iterator I = inst_begin(fun), E = inst_end(fun); I != E; I++) {
    Instruction *i = &*I;
    const InsInfo *ii = getInsInfo(i);
    if (ii->isSliced())
      continue;
    BasicBlock *BB = i->getParent();
#ifdef DEBUG_BC
    errs() << "  ";
    i->print(errs());
    errs() << " -> bb=" << BB->getNameStr() << '\n';
#endif
    PostDominanceFrontier &PDF = MP->getAnalysis<PostDominanceFrontier>(fun);
    PostDominanceFrontier::const_iterator frontier = PDF.find(BB);
    if (frontier == PDF.end())
      continue;
    changed |= updateRCSC(frontier->second.begin(), frontier->second.end());
  }
#ifdef DEBUG_BC
  errs() << __func__ << " ============ END\n";
#endif
  return changed;
}

bool FunctionStaticSlicer::updateRCSC(
                PostDominanceFrontier::DomSetType::const_iterator start,
                PostDominanceFrontier::DomSetType::const_iterator end) {
  bool changed = false;
#ifdef DEBUG_RC
  errs() << __func__ << " ============ BEG\n";
#endif
  for (; start != end; start++) {
    const BasicBlock *BB = *start;
    const Instruction &i = BB->back();
    InsInfo *ii = getInsInfo(&i);
    /* SC = BC \cup ... */
#ifdef DEBUG_SLICING
    errs() << "XXXXXXXXXXXXXX ";
    i.print(errs());
    errs() << '\n';
#endif
    ii->deslice();
    /* RC = ... \cup \cup(b \in BC) RB */
    for (ValSet::const_iterator II = ii->REF_begin(), EE = ii->REF_end();
         II != EE; II++)
      if (ii->addRC(*II)) {
        changed = true;
#ifdef DEBUG_RC
        errs() << "  added " << (*II)->getNameStr() << "\n";
#endif
      }
  }
#ifdef DEBUG_RC
  errs() << __func__ << " ============ END: changed=" << changed << "\n";
#endif
  return changed;
}

static bool canSlice(const Instruction &i) {
  switch (i.getOpcode()) {
  case Instruction::Alloca:
  case Instruction::Ret:
  case Instruction::Unreachable:
    return false;
  case Instruction::Br:
  case Instruction::Switch:
    return false;
/*    const BranchInst *bi = cast<const BranchInst>(i);
    if (bi->isUnconditional())
      return false;
    if (bi->getCondition() != UndefValue::get(bi->getType()))
      return false;*/
  }
  return true;
}

void FunctionStaticSlicer::dump() {
#ifdef DEBUG_DUMP
  for (inst_iterator I = inst_begin(fun), E = inst_end(fun); I != E; I++) {
    const Instruction &i = *I;
    const InsInfo *ii = getInsInfo(&i);
    i->print(errs());
    errs() << "\n    ";
    if (!ii->isSliced() || !canSlice(i))
      errs() << "UN";
    errs() << "SLICED\n    DEF:\n";
    for (ValSet::const_iterator II = ii->DEF_begin(), EE = ii->DEF_end();
         II != EE; II++)
      errs() << "      " << (*II)->getNameStr() << '\n';
    errs() << "    REF:\n";
    for (ValSet::const_iterator II = ii->REF_begin(), EE = ii->REF_end();
         II != EE; II++)
      errs() << "      " << (*II)->getNameStr() << '\n';
    errs() << "    RC:\n";
    for (ValSet::const_iterator II = ii->RC_begin(), EE = ii->RC_end();
         II != EE; II++)
      errs() << "      " << (*II)->getNameStr() << '\n';
  }
#endif
}

/**
 * this method calculates the static slice for the CFG
 */
void FunctionStaticSlicer::calculateStaticSlice() {
#ifdef DEBUG_SLICE
  errs() << __func__ << " ============ BEG\n";
#endif
  do {
#ifdef DEBUG_SLICE
    errs() << __func__ << " ======= compute RC\n";
#endif
    computeRC();
#ifdef DEBUG_SLICE
    errs() << __func__ << " ======= compute SC\n";
#endif
    computeSC();

#ifdef DEBUG_SLICE
    errs() << __func__ << " ======= compute BC\n";
#endif
  } while (computeBC());

  dump();

#ifdef DEBUG_SLICE
  errs() << __func__ << " ============ END\n";
#endif
}

bool FunctionStaticSlicer::slice() {
#ifdef DEBUG_SLICE
  errs() << __func__ << " ============ BEG\n";
#endif
  bool removed = false;
  for (inst_iterator I = inst_begin(fun), E = inst_end(fun); I != E;) {
    Instruction &i = *I;
    InsInfoMap::iterator ii_iter = insInfoMap.find(&i);
    assert(ii_iter != insInfoMap.end());
    const InsInfo *ii = ii_iter->second;
    ++I;
    if (ii->isSliced() && canSlice(i)) {
#ifdef DEBUG_SLICE
      errs() << "  removing:";
      i->print(errs());
      errs() << " from " << i.getParent()->getName() << '\n';
#endif
      i.replaceAllUsesWith(UndefValue::get(i.getType()));
      i.eraseFromParent();
      insInfoMap.erase(ii_iter);
      delete ii;

      removed = true;
    }
  }
  return removed;
}

/**
 * removeUndefBranches -- remove branches with undef condition
 *
 * These are irrelevant to the code, so may be remvoed completely with their
 * bodies.
 */
void FunctionStaticSlicer::removeUndefBranches(ModulePass *MP, Function &F) {
#ifdef DEBUG_SLICE
  errs() << __func__ << " ============ Removing unused branches\n";
#endif
  PostDominatorTree &PDT = MP->getAnalysis<PostDominatorTree>(F);
  for (Function::iterator I = F.begin(), E = F.end(); I != E; ++I) {
    BasicBlock &bb = *I;
    if (std::distance(succ_begin(&bb), succ_end(&bb)) <= 1)
      continue;
    Instruction &back = bb.back();
    if (back.getOpcode() != Instruction::Br &&
        back.getOpcode() != Instruction::Switch)
      continue;
    const Value *cond = back.getOperand(0);
    if (cond->getValueID() != Value::UndefValueVal)
      continue;
    DomTreeNode *node = PDT.getNode(&bb);
    assert(node);
    DomTreeNode *idom = node->getIDom();
    assert(idom);
/*    if (!idom)
      continue;*/
    BasicBlock *dest = idom->getBlock();
    if (!dest) /* TODO when there are nodes with noreturn calls */
      continue;
#ifdef DEBUG_SLICE
    errs() << "  considering branch: " << bb.getName() << '\n';
    errs() << "  dest=" << dest->getName() << "\n";
#endif
    BasicBlock::iterator ii(back);
    Instruction *newI = BranchInst::Create(dest);
    ReplaceInstWithInst(bb.getInstList(), ii, newI);
  }
#ifdef DEBUG_SLICE
  errs() << __func__ << " ============ END\n";
#endif
}

void llvm::slicing::findInitialCriterion(Function &F,
                                         FunctionStaticSlicer &ss) {
#ifdef DEBUG_INITCRIT
  errs() << __func__ << " ============ BEGIN\n";
#endif
  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I) {
    const Instruction *i = &*I;
#ifdef DEBUG_INITCRIT
    errs() << "  at ";
    i->print(errs());
    errs() << '\n';
#endif
    if (const StoreInst *SI = dyn_cast<const StoreInst>(i)) {
      const Value *LHS = SI->getPointerOperand();
      if (LHS->hasName() && LHS->getName().startswith("__ai_state")) {
#ifdef DEBUG_INITCRIT
        errs() << "    adding\n";
#endif
        ss.addInitialCriterion(SI, LHS);
      }
    } else if (const UnreachableInst *UI = dyn_cast<const UnreachableInst>(i)) {
#ifdef DEBUG_INITCRIT
      errs() << "    unreach at: " << UI->getParent()->getParent()->getName() << '\n';
#endif
      ss.addInitialCriterion(UI);
    } else if (const CallInst *CI = dyn_cast<const CallInst>(i)) {
      Function *callie = CI->getCalledFunction();
      if (callie && callie->getName().equals("__assert_fail")) {
#ifdef DEBUG_INITCRIT
        errs() << "    adding\n";
#endif
        ss.addInitialCriterion(CI);
      }
    }
  }
#ifdef DEBUG_INITCRIT
  errs() << __func__ << " ============ END\n";
#endif
}

#if 0
static void writeCFG(std::string suffix, Function &F) {
  std::string info;
  std::string filename = "/tmp/" + F.getNameStr() + "_" + suffix + ".dot";
  llvm::raw_fd_ostream O(filename.c_str(), info);
  if (!info.empty()) {
    errs() << __func__ << ": writing of '" << filename << "' failed with: " <<
        info << '\n';
    return;
  }
  llvm::WriteGraph(O, (const Function *)&F);
  errs() << __func__ << ": written " << filename << '\n';
}
#endif

template<typename PointsToSets, typename ModifiesSets>
bool FunctionSlicer::runOnFunction(Function &F, const PointsToSets &PS,
                           const ModifiesSets &MOD) {
/*  errs() << "AT: " << F.getName() << '\n';
  if (!F.getName().equals("tty_init"))
    return false;
//  writeCFG("pre", F);
  F.viewCFG();*/

  Prepare::prepareFun(F);

  FunctionStaticSlicer ss(F, this, PS, MOD);

//  errs() << "XXX " << F.getName() << "\n";

  findInitialCriterion(F, ss);

  ss.calculateStaticSlice();

  bool sliced = ss.slice();

  if (sliced)
    FunctionStaticSlicer::removeUndefBranches(this, F);

//  F.viewCFG();
  //writeCFG("post", F);
  return sliced;
}

bool FunctionSlicer::runOnModule(Module &M) {
  ptr::PointsToSets<ptr::ANDERSEN>::Type PS;
  ptr::ProgramStructure P(M);
  computePointsToSets(P, PS);

  callgraph::Callgraph CG(M, PS);

  mods::Modifies<mods::DUMB_SPEEDY>::Type MOD;
  mods::ProgramStructure P1(M);
  computeModifies(P1, CG, PS, MOD);

  bool modified = false;
  for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
    Function &F = *I;
    if (F.isDeclaration())
      continue;
    modified |= runOnFunction(F, PS, MOD);
  }
  return modified;
}
