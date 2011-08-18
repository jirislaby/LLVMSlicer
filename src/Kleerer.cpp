#include "llvm/Constants.h"
#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Bitcode/ReaderWriter.h"
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
  Kleerer(ModulePass &modPass, Module &M) : modPass(modPass), M(M), done(false) {}

  bool run();

private:
  ModulePass &modPass;
  Module &M;
  bool done;

  void handleFun(Function &F);
  void handleBB(const BasicBlock &BB);
  bool handleIns(const Instruction &ins);

  void writeMain(Function &F);
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

static void check(Value *Func, ArrayRef<Value *> Args) {
  FunctionType *FTy =
    cast<FunctionType>(cast<PointerType>(Func->getType())->getElementType());

  assert((Args.size() == FTy->getNumParams() ||
          (FTy->isVarArg() && Args.size() > FTy->getNumParams())) &&
         "XXCalling a function with bad signature!");

  for (unsigned i = 0; i != Args.size(); ++i) {
    if (!(FTy->getParamType(i) == Args[i]->getType())) {
      errs() << "types:\n  ";
      FTy->getParamType(i)->dump();
      errs() << "\n  ";
      Args[i]->getType()->dump();
      errs() << "\n";
    }
    assert((i >= FTy->getNumParams() ||
            FTy->getParamType(i) == Args[i]->getType()) &&
           "YYCalling a function with a bad signature!");
  }
}

void Kleerer::writeMain(Function &F) {
  LLVMContext &C = M.getContext();
  std::string name = "main." + F.getNameStr() + ".o";
  Module mainMod(name, C);
  Function *mainFun = Function::Create(TypeBuilder<int(), false>::get(C),
                   GlobalValue::ExternalLinkage, "main", &mainMod);
  BasicBlock *mainBB = BasicBlock::Create(C, "entry", mainFun);
  BasicBlock::InstListType &insList = mainBB->getInstList();

//  F.dump();
  std::vector<Value *> params;
  for (Function::const_arg_iterator I = F.arg_begin(), E = F.arg_end(); I != E;
       ++I) {
    const Value &param = *I;
    Type *type = param.getType();
    errs() << "param\n";
    param.dump();
    Value *val;
    Instruction *ins;
    if (const PointerType *PT = dyn_cast<const PointerType>(type)) {
      insList.push_back(ins = new AllocaInst(PT->getElementType()));
      val = ins;
    } else if (IntegerType *IT = dyn_cast<IntegerType>(type)) {
      insList.push_back(ins = new AllocaInst(IT));
      insList.push_back(ins = new LoadInst(ins));
      val = ins;
    }
    if (val)
      params.push_back(val);
  }
//  mainFun->viewCFG();

  check(&F, params);

  CallInst::Create(&F, params, "", mainBB);
  ReturnInst::Create(C, ConstantInt::get(mainFun->getReturnType(), 0),
                     mainBB);

  mainFun->viewCFG();

  std::string ErrorInfo;
  raw_fd_ostream out(name.c_str(), ErrorInfo);
  if (!ErrorInfo.empty()) {
    errs() << __func__ << ": cannot write '" << name << "'!\n";
    return;
  }
//  WriteBitcodeToFile(&mainMod, out);
  out << mainMod;
  errs() << __func__ << ": written: '" << name << "'\n";
//  done = true;
}

void Kleerer::handleFun(Function &F) {
/*  for (Function::const_iterator I = F.begin(), E = F.end(); I != E; ++I)
    handleBB()*/
  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I)
    if (handleIns(*I)) {
      writeMain(F);
      break;
    }
}

bool Kleerer::run() {
  for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
    Function &F = *I;
    if (F.isDeclaration())
      continue;
    handleFun(F);
    if (done)
      break;
  }
  return false;
}

bool KleererPass::runOnModule(Module &M) {
  Kleerer K(*this, M);
  return K.run();
}
