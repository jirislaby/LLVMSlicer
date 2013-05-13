// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/raw_ostream.h"

#include "Callgraph/Callgraph.h"
#include "PointsTo/PointsTo.h"
#include "Slicing/Prepare.h"

using namespace llvm;

namespace {
  class ModStats : public ModulePass {
  public:
    static char ID;

    ModStats() : ModulePass(ID) { }

    virtual bool runOnModule(Module &M);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      AU.addRequired<LoopInfo>();
    }
  };
}

class FunInfo {
public:
  FunInfo(const Function &F) : F(F), ins(0), _hasAsm(false), _hasCall(false),
          _hasExternalCall(false), _hasLock(false), _hasLoop(false),
          _hasNestedAsm(false), _hasNestedExtCall(false), _hasNestedLock(false),
          _hasNestedLoop(false) {}

  const Function &getFun() const { return F; }

  void inline incIns() { ins++; }
  void inline setHasAsm() { _hasAsm = true; }
  void inline setHasCall() { _hasCall = true; }
  void inline setHasExternalCall() { _hasExternalCall = true; }
  void inline setHasLock() { _hasLock = true; }
  void inline setHasLoop() { _hasLoop = true; }
  void inline setHasNestedAsm() { _hasNestedAsm = true; }
  void inline setHasNestedExtCall() { _hasNestedExtCall = true; }
  void inline setHasNestedLock() { _hasNestedLock = true; }
  void inline setHasNestedLoop() { _hasNestedLoop = true; }

  unsigned getIns() const { return ins; }
  bool hasAsm() const { return _hasAsm; }
  bool hasCall() const { return _hasCall; }
  bool hasExternalCall() const { return _hasExternalCall; }
  bool hasLock() const { return _hasLock; }
  bool hasLoop() const { return _hasLoop; }
  bool hasNestedAsm() const { return _hasAsm || _hasNestedAsm; }
  bool hasNestedExtCall() const { return _hasExternalCall || _hasNestedExtCall; }
  bool hasNestedLock() const { return _hasLock || _hasNestedLock; }
  bool hasNestedLoop() const { return _hasLoop || _hasNestedLoop; }

private:
  const Function &F;

  unsigned ins;
  bool _hasAsm;
  bool _hasCall;
  bool _hasExternalCall;
  bool _hasLock;
  bool _hasLoop;
  bool _hasNestedAsm;
  bool _hasNestedExtCall;
  bool _hasNestedLock;
  bool _hasNestedLoop;
};

class ModInfo {
public:
  typedef std::map<const Function *, FunInfo *> FunMap;

  ModInfo(const Module &M) : M(M), ins(0), fun(0), funWithAsm(0),
          funWithCall(0), funWithExtCall(0), funWithLock(0), funWithLoop(0),
          funWithNestedAsm(0), funWithNestedExtCall(0), funWithNestedLock(0),
          funWithNestedLoop(0), funSafe(0), funSafeWOLoop(0) {}

  void inline addIns(unsigned ins) { this->ins += ins; }
  void inline incFun() { fun++; }
  void inline incFunWithAsm() { funWithAsm++; }
  void inline incFunWithCall() { funWithCall++; }
  void inline incFunWithExtCall() { funWithExtCall++; }
  void inline incFunWithLock() { funWithLock++; }
  void inline incFunWithLoop() { funWithLoop++; }
  void inline incFunWithNestedAsm() { funWithNestedAsm++; }
  void inline incFunWithNestedExtCall() { funWithNestedExtCall++; }
  void inline incFunWithNestedLock() { funWithNestedLock++; }
  void inline incFunWithNestedLoop() { funWithNestedLoop++; }
  void inline incFunSafe() { funSafe++; }
  void inline incFunSafeWOLoop() { funSafeWOLoop++; }

  void addFunInfo(FunInfo *funInfo) {
    funMap.insert(FunMap::value_type(&funInfo->getFun(), funInfo));
  }
  FunInfo *getFunInfo(const Function *fun) { return funMap[fun]; }

  void dump() const;

private:
  const Module &M;

  unsigned ins;
  unsigned fun;
  unsigned funWithAsm;
  unsigned funWithCall;
  unsigned funWithExtCall;
  unsigned funWithLock;
  unsigned funWithLoop;
  unsigned funWithNestedAsm;
  unsigned funWithNestedExtCall;
  unsigned funWithNestedLock;
  unsigned funWithNestedLoop;
  unsigned funSafe;
  unsigned funSafeWOLoop;
  FunMap funMap;
};

class StatsComputer {
public:
  StatsComputer(ModulePass &modPass, Module &M) : modPass(modPass), M(M) {}

  void run();

private:
  ModulePass &modPass;
  Module &M;

  void handleFun(ModInfo &modInfo, const Function &F, const LoopInfo &LI);
  void handleBB(FunInfo &funInfo, const LoopInfo &LI, const BasicBlock &BB);
  void handleIns(FunInfo &funInfo, const Instruction &ins);
};

static RegisterPass<ModStats> X("modstats", "Prints out some stats about module");
char ModStats::ID;

void ModInfo::dump() const {
  errs() << "Module " << M.getModuleIdentifier() << " dump\n";
  errs() << "  machine code: " << M.getModuleIdentifier() << " " << ins <<
    " " << fun << " " <<
    funWithAsm << " " << funWithNestedAsm << " " <<
    funWithExtCall << " " << funWithNestedExtCall << " " <<
    funWithLock << " " << funWithNestedLock << " " <<
    funWithLoop << " " << funWithNestedLoop << " " <<
    funSafe << " " << funSafeWOLoop << "\n";
  errs() << "  instructions: " << ins << "\n";
  errs() << "  functions: " << fun << "\n";
  errs() << "    with asm: " << funWithAsm << "\n";
  errs() << "      incl nested: " << funWithNestedAsm << "\n";
  errs() << "    with ext call: " << funWithExtCall << "\n";
  errs() << "      incl nested: " << funWithNestedExtCall << "\n";
  errs() << "    with lock: " << funWithLock << "\n";
  errs() << "      incl nested: " << funWithNestedLock << "\n";
  errs() << "    with loop: " << funWithLoop << "\n";
  errs() << "      incl nested: " << funWithNestedLoop << "\n";
  errs() << "    safe: " << funSafe << "\n";
  errs() << "    safe w/o loop: " << funSafeWOLoop << "\n";
}

static bool isLockingFun(StringRef name) {
  return name.startswith("_spin_lock") || name.startswith("_spin_unlock") ||
    name.startswith("_spin_trylock") ||
    name.startswith("_read_lock") || name.startswith("_read_unlock") ||
    name.startswith("_read_trylock") ||
    name.startswith("_write_lock") || name.startswith("_write_unlock") ||
    name.startswith("_write_trylock") ||
    name.equals("mutex_lock") || name.equals("mutex_unlock") ||
    name.equals("mutex_lock_interruptible") || name.equals("mutex_trylock");
}

void StatsComputer::handleIns(FunInfo &funInfo, const Instruction &ins) {
  if (const CallInst *CI = dyn_cast<const CallInst>(&ins)) {
    if (CI->isInlineAsm()) {
#ifdef DEBUG_ASM
      errs() << "ASM: in " << ins.getParent()->getParent()->getName() << " ";
      CI->print(errs());
      CI->getParent()->print(errs());
      errs() << "\n";
#endif
      funInfo.setHasAsm();
    } else {
      Function *called = CI->getCalledFunction();
      if (called) {
        StringRef calledName = called->getName();
        if (calledName.startswith("llvm.") ||
            calledName.equals("__assert_fail") ||
            calledName.equals("__kmalloc") || calledName.equals("kfree") ||
            isLockingFun(calledName))
          return;
        if (called->isDeclaration()) {
#ifdef DEBUG_EXT
          errs() << "EXT1 " << ins.getParent()->getParent()->getName() <<
            " to " << called->getName() << "\n";
#endif
          funInfo.setHasExternalCall();
        }
      } else {
#ifdef DEBUG_EXT
        errs() << "EXT2 " << ins.getParent()->getParent()->getName() << " to ";
        ins.print(errs());
        errs() << '\n';
#endif
        funInfo.setHasExternalCall();
      }
      funInfo.setHasCall();
    }
  } else if (const StoreInst *SI = dyn_cast<const StoreInst>(&ins)) {
    const Value *LHS = SI->getPointerOperand();
    if (LHS->hasName() && LHS->getName().startswith("__ai_state"))
      funInfo.setHasLock();
  }
}

void StatsComputer::handleBB(FunInfo &funInfo, const LoopInfo &LI,
                             const BasicBlock &BB) {
  for (BasicBlock::const_iterator I = BB.begin(), E = BB.end(); I != E; ++I) {
    funInfo.incIns();
    if (LI.getLoopFor(&BB))
      funInfo.setHasLoop();
    handleIns(funInfo, *I);
  }
}

void StatsComputer::handleFun(ModInfo &modInfo, const Function &F,
                              const LoopInfo &LI) {
  FunInfo *funInfo = new FunInfo(F);
  modInfo.addFunInfo(funInfo);

  for (Function::const_iterator I = F.begin(), E = F.end(); I != E; ++I)
    handleBB(*funInfo, LI, *I);
}

static std::string __attribute__((unused))
getFlags(const FunInfo *funInfo, bool nested = false) {
  std::string flags;
  if (funInfo->hasAsm() || (nested && funInfo->hasNestedAsm()))
    flags += "A";
  if (funInfo->hasExternalCall() || (nested && funInfo->hasNestedExtCall()))
    flags += "E";
  if (funInfo->hasLock() || (nested && funInfo->hasNestedLock()))
    flags += "L";
  if (funInfo->hasLoop() || (nested && funInfo->hasNestedLoop()))
    flags += "O";
  return flags;
}

void StatsComputer::run() {
  ptr::PointsToSets<ptr::ANDERSEN>::Type PS;
  {
    ptr::ProgramStructure P(M);
    computePointsToSets(P, PS);
  }

  callgraph::Callgraph CG(M, PS);
  ModInfo modInfo(M);

#ifdef DEBUG_DUMP_CALLREL
  for (callgraph::Callgraph::const_iterator I = CG.begin_closure(),
		  E = CG.end_closure(); I != E; ++I) {
	  const Function *from = I->first;
	  const Function *to = I->second;
	  errs() << "CALLREL " << from->getName() << " => " << to->getName() << "\n";
  }
#endif

  for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
    Function &F = *I;
    if (F.isDeclaration())
      continue;
    handleFun(modInfo, F, modPass.getAnalysis<LoopInfo>(F));
  }

  const ConstantArray *initFuns = getInitFuns(M);
  assert(initFuns && "No initial functions found. Did you run -prepare?");

  for (ConstantArray::const_op_iterator I = initFuns->op_begin(),
       E = initFuns->op_end(); I != E; ++I) {
    const ConstantExpr *CE = cast<ConstantExpr>(&*I);
    assert(CE->getOpcode() == Instruction::BitCast);
    const Function &F = *cast<Function>(CE->getOperand(0));
    FunInfo *funInfo = modInfo.getFunInfo(&F);
    callgraph::Callgraph::const_iterator II, EE;
    llvm::tie(II, EE) = CG.calls(&F);
#ifdef DEBUG_NESTED
    errs() << "at " << F.getName() << " flags [" << getFlags(funInfo) << "]\n";
#endif
    for (; II != EE; ++II) {
      const Function *called = II->second;
      const FunInfo *calledFunInfo = modInfo.getFunInfo(called);
#ifdef DEBUG_NESTED
      errs() << "  " << called->getName() << " [" << getFlags(calledFunInfo) <<
          "]\n";
#endif
      if (calledFunInfo->hasAsm())
        funInfo->setHasNestedAsm();
      if (calledFunInfo->hasExternalCall())
        funInfo->setHasNestedExtCall();
      if (calledFunInfo->hasLock())
        funInfo->setHasNestedLock();
      if (calledFunInfo->hasLoop())
        funInfo->setHasNestedLoop();
    }
#ifdef DEBUG_NESTED
    errs() << "RET flags [" << getFlags(funInfo, true) << "]\n";
#endif

    if (funInfo->hasNestedLock()) {
      modInfo.incFun();
      modInfo.addIns(funInfo->getIns());
      if (funInfo->hasAsm())
        modInfo.incFunWithAsm();
      if (funInfo->hasCall())
        modInfo.incFunWithCall();
      if (funInfo->hasExternalCall())
        modInfo.incFunWithExtCall();
      if (funInfo->hasLock())
        modInfo.incFunWithLock();
      if (funInfo->hasLoop())
        modInfo.incFunWithLoop();
      if (funInfo->hasNestedAsm())
        modInfo.incFunWithNestedAsm();
      if (funInfo->hasNestedExtCall())
        modInfo.incFunWithNestedExtCall();
      if (funInfo->hasNestedLock())
        modInfo.incFunWithNestedLock();
      if (funInfo->hasNestedLoop())
        modInfo.incFunWithNestedLoop();
      if (!funInfo->hasNestedExtCall() && !funInfo->hasNestedAsm()) {
        modInfo.incFunSafe();
        if (!funInfo->hasNestedLoop())
          modInfo.incFunSafeWOLoop();
      }
    }
  }

  modInfo.dump();
}

bool ModStats::runOnModule(Module &M) {
  StatsComputer sc(*this, M);
  sc.run();
	return false;
}
