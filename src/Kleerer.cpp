#include "llvm/Pass.h"
#include "llvm/Module.h"
//#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/TypeBuilder.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {
  class KleererPass : public ModulePass {
  public:
    static char ID;

    KleererPass() : ModulePass(ID) { }

    virtual bool runOnModule(Module &M);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
//      AU.addRequired<LoopInfo>();
    }
  };
}

class Kleerer {
public:
  Kleerer(ModulePass &modPass, Module &M) : modPass(modPass), M(M) {}

  bool run();

private:
  ModulePass &modPass;
  Module &M;

  void handleFun(const Function &F);
  void handleBB(const BasicBlock &BB);
  bool handleIns(const Instruction &ins);

  void writeMain(const Function &F);
};

static RegisterPass<KleererPass> X("kleerer", "Prepares a module for Klee");
char KleererPass::ID;

bool Kleerer::handleIns(const Instruction &ins) {
  switch (ins.getOpcode()) {
  case Instruction::Store:
    const Value *LHS = ins.getOperand(1);
    if (LHS->hasName() && LHS->getName().equals("__ai_state"))
      return true;
    break;
  }
  return false;
}

#if 0
void Kleerer::writeMain(const Function &F) {
  std::string Filename = "main." + M.getModuleIdentifier() + ".c";
  std::string ErrorInfo;
  raw_fd_ostream File(Filename.c_str(), ErrorInfo);
  if (!ErrorInfo.empty()) {
    errs() << __func__ << ": cannot write '" << Filename << "'!\n";
    return;
  }
  File <<
}
#endif

void Kleerer::writeMain(const Function &F) {
  LLVMContext &C = M.getContext();
  Module mainMod("main." + M.getModuleIdentifier() + ".o", C);
  Function::Create(TypeBuilder<int(), false>::get(C),
                   GlobalValue::ExternalLinkage, "main", &mainMod);
}

void Kleerer::handleFun(const Function &F) {
/*  for (Function::const_iterator I = F.begin(), E = F.end(); I != E; ++I)
    handleBB()*/
  for (const_inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I)
    if (handleIns(*I)) {
      writeMain(F);
      break;
    }
}

bool Kleerer::run() {
  for (Module::const_iterator I = M.begin(), E = M.end(); I != E; ++I) {
    const Function &F = *I;
    if (F.isDeclaration())
      continue;
    handleFun(F);
  }
  return false;
}

bool KleererPass::runOnModule(Module &M) {
  Kleerer K(*this, M);
  return K.run();
}
