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
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TypeBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "PostDominanceFrontier.h"
#include "Callgraph/Callgraph.h"
#include "Callgraph/LangLLVM.h"
#include "Modifies/LangLLVM.h"
#include "Modifies/Modifies.h"
#include "Modifies/AlgoDumbSpeedy.h"
#include "PointsTo/AlgoAndersen.h"
#include "PointsTo/LangLLVM.h"
#include "PointsTo/PointsTo.h"

using namespace llvm;

typedef llvm::SmallSetVector<const Value *, 10> ValSet;

class InsInfo {
public:
  InsInfo(const Instruction *i);

  bool addRC(const Value *var) { return RC.insert(var); }
  void deslice() { sliced = false; }

  ValSet::const_iterator RC_begin() const { return RC.begin(); }
  ValSet::const_iterator RC_end() const { return RC.end(); }
  ValSet::const_iterator DEF_begin() const { return DEF.begin(); }
  ValSet::const_iterator DEF_end() const { return DEF.end(); }
  ValSet::const_iterator REF_begin() const { return REF.begin(); }
  ValSet::const_iterator REF_end() const { return REF.end(); }

  bool isSliced() const { return sliced; }

private:
  const Instruction *ins;
  ValSet RC, DEF, REF;
  bool sliced;
};

InsInfo::InsInfo(const Instruction *i) : ins(i), sliced(true) {
  unsigned opcode = i->getOpcode();
#ifdef DEBUG_INSINFO
  errs() << __func__ << " BEG\n";
#endif
  /* whole ins is used as a value somewhere */
  if (!i->use_empty())
    DEF.insert(i);
  switch (opcode) {
  case Instruction::Store:
    DEF.insert(i->getOperand(1));
//    errs() << "  DEF: " << DEF->getNameStr() << " ID=" << DEF->getValueID() << " <- " << i->getOperand(0)->getNameStr() << '\n';
    break;
/*  case Instruction::Alloca:
  case Instruction::Load:
    DEF.insert(i);
    break;*/
  }
#ifdef DEBUG_INSINFO
  errs() << "  DEF:" << '\n';
  for (ValSet::const_iterator I = DEF.begin(), E = DEF.end(); I != E; I++)
    errs() << "    " << (*I)->getNameStr() << " ID=" << (*I)->getValueID() << '\n';
  errs() << "  OP:\n";
#endif
  int op = 0;
  for (Instruction::const_op_iterator I = i->op_begin(), E = i->op_end();
       I != E; I++, op++) {
    if (opcode == Instruction::Store && op == 1)
      continue;
    const Value *var = *I;
    if (var->hasName())
      REF.insert(var);
#ifdef DEBUG_INSINFO
    errs() << "    " << (var->hasName() ? var->getNameStr() : "<noname>") << ": ID=" << var->getValueID();
    if (const ConstantInt *cint = dyn_cast<const ConstantInt>(var))
      errs() << " val=" << cint->getValue().toString(10, true);
    if (const Instruction *ins = dyn_cast<const Instruction>(var))
      errs() << " opcode=" << ins->getOpcodeName();
    errs() << '\n';
#endif
  }
#ifdef DEBUG_INSINFO
  errs() << __func__ << " END\n";
#endif
}

class StaticSlicer {
public:
  typedef std::map<const Instruction *, InsInfo *> InsInfoMap;

  StaticSlicer(Function &F, PostDominatorTree &PDT,
               PostDominanceFrontier &PDF) : fun(F), PDT(PDT), PDF(PDF) {}
  ~StaticSlicer();

  void addInitialCriterion(const Instruction *ins, const Value *cond = 0) {
    InsInfo *ii = getInsInfo(ins);
    if (cond)
      ii->addRC(cond);
    ii->deslice();
  }
  void calculateStaticSlice();
  bool slice();

private:
  Function &fun;
  PostDominatorTree &PDT;
  PostDominanceFrontier &PDF;
  InsInfoMap insInfoMap;

  void crawlBasicBlock(const BasicBlock *bb);
  bool computeRCi(const Instruction *i, const Instruction *j);
  bool computeRCi(const Instruction *i);
  void computeRC();

  void computeSCi(const Instruction *i, const Instruction *j);
  void computeSC();

  bool computeBC();
  bool updateRCSC(PostDominanceFrontier::DomSetType::const_iterator start,
                  PostDominanceFrontier::DomSetType::const_iterator end);

  void dump();

  InsInfo *getInsInfo(const Instruction *i) {
    InsInfoMap::const_iterator I = insInfoMap.find(i);
    if (I != insInfoMap.end())
      return I->second;
    InsInfo *insInfo = new InsInfo(i);
    insInfoMap.insert(InsInfoMap::value_type(i, insInfo));
    return insInfo;
  }
};


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
    private:
      bool runOnFunction(Function &F);
      void findInitialCriterion(Function &F, StaticSlicer &ss);
  };
}

static RegisterPass<Slicer> X("slice", "Slices the code");
char Slicer::ID;

StaticSlicer::~StaticSlicer() {
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
bool StaticSlicer::computeRCi(const Instruction *i, const Instruction *j) {
  InsInfo *insInfoi = getInsInfo(i), *insInfoj = getInsInfo(j);
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

bool StaticSlicer::computeRCi(const Instruction *i) {
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
  for (SuccList::const_iterator I = succList.begin(), E = succList.end();
       I != E; I++)
    changed |= computeRCi(i, *I);

  return changed;
}

void StaticSlicer::computeRC() {
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
void StaticSlicer::computeSCi(const Instruction *i, const Instruction *j) {
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

void StaticSlicer::computeSC() {
  for (inst_iterator I = inst_begin(fun), E = inst_end(fun); I != E; I++) {
    const Instruction *i = &*I;
    SuccList succList = getSuccList(i);
    for (SuccList::const_iterator II = succList.begin(), EE = succList.end();
         II != EE; II++)
      computeSCi(i, *II);
  }
}

bool StaticSlicer::computeBC() {
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

bool StaticSlicer::updateRCSC(
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

void StaticSlicer::dump() {
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
void StaticSlicer::calculateStaticSlice() {
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

bool StaticSlicer::slice() {
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
#ifdef DEBUG_SLICE
  errs() << __func__ << " ============ Removing unused branches\n";
#endif
  for (Function::iterator I = fun.begin(), E = fun.end(); I != E; ++I) {
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
  return removed;
}

void Slicer::findInitialCriterion(Function &F, StaticSlicer &ss) {
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

static GlobalVariable *getAiVar(Function &F, const CallInst *CI) {
  const ConstantExpr *GEP =
    dyn_cast<const ConstantExpr>(CI->getOperand(0));
  assert(GEP && GEP->getOpcode() == Instruction::GetElementPtrInst);
  const GlobalVariable *strVar =
    dyn_cast<const GlobalVariable>(GEP->getOperand(0));
  assert(strVar && strVar->hasInitializer());
  const ConstantArray *str =
    dyn_cast<const ConstantArray>(strVar->getInitializer());
  assert(str && str->isCString());
  std::string id = str->getAsCString();
  char *cstr = new char[11 + id.size() + 1];
  strcpy(cstr, "__ai_state_"); /* len=11 */
  strcpy(cstr + 11, id.c_str());
  for (size_t i = 11; i < 11 + id.size(); i++)
    if (cstr[i] != '_' && !isupper(cstr[i]) && !islower(cstr[i]))
      cstr[i] = 'X';
  Type *intType = TypeBuilder<int, false>::get(F.getContext());
  GlobalVariable *glob =
    dyn_cast<GlobalVariable>(F.getParent()->getOrInsertGlobal(cstr, intType));
  delete cstr;
  return glob;
}

static void replaceInsTrans(Function &F, CallInst *CI) {
  Type *intType = TypeBuilder<int, false>::get(F.getContext());
//  errs() << __func__ << ": ======\n";
  GlobalVariable *glob = getAiVar(F, CI);
//  errs() << "\nid=" << glob->getValueID() << "\n";
  glob->setInitializer(ConstantInt::get(intType, 0));
  ReplaceInstWithInst(CI, new StoreInst(CI->getOperand(1), glob, true));
}

static void replaceInsCheck(Function &F, CallInst *CI) {
  errs() << __func__ << ": ======\n";
  CI->dump();
  GlobalVariable *glob = getAiVar(F, CI);
  errs() << "\nVAR=";
  glob->dump();
  errs() << " compare to=";
  CI->getOperand(1)->dump();
  errs() << "\n";
  BasicBlock *CIBB = CI->getParent();
  BasicBlock *contBB = CIBB->splitBasicBlock(BasicBlock::iterator(CI));
  BasicBlock *assBB = BasicBlock::Create(F.getContext(), "assertBlk", &F);
  new UnreachableInst(F.getContext(), assBB);
  CI->eraseFromParent();
  Value *ai_stateVal = new LoadInst(glob, "", true, 4, CIBB);
  Value *ai_stateIsEq = new ICmpInst(*CIBB, CmpInst::ICMP_EQ, ai_stateVal,
                                     CI->getOperand(1));
  BranchInst::Create(contBB, assBB, ai_stateIsEq, CIBB);
  F.viewCFG();
}

static void prepareFun(Function &F) {
//  F.dump();
  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E;) {
    Instruction *ins = &*I;
    ++I;
    if (CallInst *CI = dyn_cast<CallInst>(ins)) {
      Function *callee = CI->getCalledFunction();
      if (callee) {
        StringRef calleeName = callee->getName();
        if (calleeName.equals("__ai_trans"))
          replaceInsTrans(F, CI);
        else if (calleeName.equals("__ai_check_eq"))
          replaceInsCheck(F, CI);
      }
    }
  }
//  F.dump();
}

bool Slicer::runOnFunction(Function &F) {
/*  errs() << "AT: " << F.getName() << '\n';
  if (!F.getName().equals("tty_init"))
    return false;
//  writeCFG("pre", F);
  F.viewCFG();*/
  PostDominanceFrontier &PDF = getAnalysis<PostDominanceFrontier>(F);
  PostDominatorTree &PDT = getAnalysis<PostDominatorTree>(F);

  prepareFun(F);

  StaticSlicer ss(F, PDT, PDF);

//  errs() << "XXX " << F.getName() << "\n";

  findInitialCriterion(F, ss);

  ss.calculateStaticSlice();

  bool sliced = ss.slice();

//  F.viewCFG();
  //writeCFG("post", F);
  return sliced;
}

bool Slicer::runOnModule(Module &M) {
  ptr::PointsToSets<LLVM,ptr::ANDERSEN>::Type PS;
  ptr::ProgramStructure<LLVM,AlgorithmProperties<ptr::ANDERSEN>::Type>::Type P(M);
  computePointsToSets(P,PS);

  callgraph::Callgraph<LLVM>::Type CG(M, PS);

  mods::Modifies<LLVM, mods::DUMB_SPEEDY>::Type MOD;
  mods::ProgramStructure<LLVM, AlgorithmProperties<mods::DUMB_SPEEDY>::Type>::Type
      P1(M);
  computeModifies(P1, CG, PS, MOD);

  bool modified = false;
  for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
    Function &F = *I;
    if (F.isDeclaration())
      continue;
    modified |= runOnFunction(F);
  }
  return modified;
}
