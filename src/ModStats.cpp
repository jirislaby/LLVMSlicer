#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/raw_ostream.h"

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

class ModInfo {
public:
  ModInfo(const Module &M) : M(M), ins(0), fun(0), funWithAsm(0),
          funWithCall(0), funWithLock(0), funWithLoop(0), funSafe(0),
          funSafeWOLoop(0) {}

  void inline addIns(unsigned ins) { this->ins += ins; }
  void inline incFun() { fun++; }
  void inline incFunWithAsm() { funWithAsm++; }
  void inline incFunWithCall() { funWithCall++; }
  void inline incFunWithLock() { funWithLock++; }
  void inline incFunWithLoop() { funWithLoop++; }
  void inline incFunSafe() { funSafe++; }
  void inline incFunSafeWOLoop() { funSafeWOLoop++; }

  void dump() const;

private:
  const Module &M;

  unsigned ins;
  unsigned fun;
  unsigned funWithAsm;
  unsigned funWithCall;
  unsigned funWithLock;
  unsigned funWithLoop;
  unsigned funSafe;
  unsigned funSafeWOLoop;
};

class FunInfo {
public:
  FunInfo(const Function &F) : F(F), ins(0), _hasAsm(false), _hasCall(false),
          _hasLock(false), _hasLoop(false) {}

  void inline incIns() { ins++; }
  void inline setHasAsm() { _hasAsm = true; }
  void inline setHasCall() { _hasCall = true; }
  void inline setHasLock() { _hasLock = true; }
  void inline setHasLoop() { _hasLoop = true; }

  unsigned getIns() const { return ins; }
  bool hasAsm() const { return _hasAsm; }
  bool hasCall() const { return _hasCall; }
  bool hasLock() const { return _hasLock; }
  bool hasLoop() const { return _hasLoop; }

private:
  const Function &F;

  unsigned ins;
  bool _hasAsm;
  bool _hasCall;
  bool _hasLock;
  bool _hasLoop;
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
    " " << fun << " " << funWithAsm << " " << funWithCall <<
    " " << funWithLock << " " << funWithLoop << " " << funSafe <<
    " " << funSafeWOLoop << "\n";
  errs() << "  instructions: " << ins << "\n";
  errs() << "  functions: " << fun << "\n";
  errs() << "    with asm: " << funWithAsm << "\n";
  errs() << "    with call: " << funWithCall << "\n";
  errs() << "    with lock: " << funWithLock << "\n";
  errs() << "    with loop: " << funWithLoop << "\n";
  errs() << "    safe: " << funSafe << "\n";
  errs() << "    safe w/o loop: " << funSafeWOLoop << "\n";
}

void StatsComputer::handleIns(FunInfo &funInfo, const Instruction &ins) {
  switch (ins.getOpcode()) {
  case Instruction::Call:
    if (ins.getOperand(0)->getValueID() == Value::InlineAsmVal)
      funInfo.setHasAsm();
    else
      funInfo.setHasCall();
    break;
  case Instruction::Store:
    const Value *LHS = ins.getOperand(1);
    if (LHS->hasName() && LHS->getName().equals("__ai_state"))
      funInfo.setHasLock();
    break;
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
  FunInfo funInfo(F);

  for (Function::const_iterator I = F.begin(), E = F.end(); I != E; ++I)
    handleBB(funInfo, LI, *I);

  if (funInfo.hasLock()) {
    modInfo.incFun();
    modInfo.addIns(funInfo.getIns());
    if (funInfo.hasAsm())
      modInfo.incFunWithAsm();
    if (funInfo.hasCall())
      modInfo.incFunWithCall();
    if (funInfo.hasLock())
      modInfo.incFunWithLock();
    if (funInfo.hasLoop())
      modInfo.incFunWithLoop();
    if (!funInfo.hasCall() && !funInfo.hasAsm()) {
      modInfo.incFunSafe();
      if (!funInfo.hasLoop())
        modInfo.incFunSafeWOLoop();
    }
  }
}

void StatsComputer::run() {
  ModInfo modInfo(M);

  for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
    Function &F = *I;
    if (F.isDeclaration())
      continue;
    handleFun(modInfo, F, modPass.getAnalysis<LoopInfo>(F));
  }

  modInfo.dump();
}

bool ModStats::runOnModule(Module &M) {
  StatsComputer sc(*this, M);
  sc.run();
	return false;
}
