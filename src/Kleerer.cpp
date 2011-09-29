#include "llvm/Constants.h"
#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/PassManager.h"
#include "llvm/Module.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/TypeBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetData.h"

using namespace llvm;

namespace {
  class KleererPass : public ModulePass {
  public:
    static char ID;

    KleererPass() : ModulePass(ID) { }

    virtual bool runOnModule(Module &M);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      AU.addRequired<TargetData>();
    }
  };
}

class Kleerer {
public:
  Kleerer(ModulePass &modPass, Module &M, TargetData &TD) : modPass(modPass),
      M(M), TD(TD), C(M.getContext()), intPtrTy(TD.getIntPtrType(C)),
      done(false) {
    voidPtrType = TypeBuilder<void *, false>::get(C);
    intType = TypeBuilder<int, false>::get(C);
    uintType = TypeBuilder<unsigned, false>::get(C);
  }

  bool run();

private:
  ModulePass &modPass;
  Module &M;
  TargetData &TD;
  LLVMContext &C;
  IntegerType *intPtrTy;
  bool done;

  /* types */
  Type *voidPtrType;
  Type *intType;
  Type *uintType;

  void handleFun(Function &F);
  void handleBB(const BasicBlock &BB);
  int handleIns(const Instruction &ins);

  void writeMain(Function &F);

  Constant *get_assert_fail();

  Instruction *createMalloc(BasicBlock *BB, Type *type, Value *arraySize);
  Instruction *call_klee_make_symbolic(Function *klee_make_symbolic,
                                       Constant *noname, BasicBlock *BB,
                                       Type *type, Value *addr,
                                       Value *arraySize = 0);
  void makeAiStateSymbolic(Function *klee_make_symbolic, Module &M,
                           BasicBlock *BB, Constant *noname);
  BasicBlock *checkAiState(Function *mainFun, BasicBlock *BB, Constant *noname);
  void addGlobals(Module &M);
};

static RegisterPass<KleererPass> X("kleerer", "Prepares a module for Klee");
char KleererPass::ID;

/**
 * @return: 1=important fun, 0=no idea, -1=do not handle
 */
int Kleerer::handleIns(const Instruction &ins) {
  switch (ins.getOpcode()) {
  case Instruction::Store: {
    const StoreInst *SI = cast<const StoreInst>(&ins);
    const Value *LHS = SI->getPointerOperand();
    if (LHS->hasName() && LHS->getName().startswith("__ai_state"))
      return 1;
    break;
  }
  case Instruction::Call: {
    const CallInst *CI = cast<const CallInst>(&ins);
    const Function *callie = CI->getCalledFunction();
    if (callie) {
      if (!callie->isDeclaration())
        return 0;
      if (callie->getName().startswith("mutex_") ||
          callie->getName().equals("__assert_fail"))
        return 0;
    }
    errs() << "in " << ins.getParent()->getParent()->getName() << " CALL: ";
    if (callie)
      errs() << callie->getName();
    if (CI->isInlineAsm())
        errs() << "is ASM";
    errs() << "  ignoring fun\n";
    return -1;
  }
  }
  return 0;
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

static unsigned getTypeSize(TargetData &TD, Type *type) {
  unsigned TypeSize = TD.getTypeAllocSize(type);

  if (StructType *ST = dyn_cast<StructType>(type))
    TypeSize = TD.getStructLayout(ST)->getSizeInBytes();

  return TypeSize;
}

Instruction *Kleerer::createMalloc(BasicBlock *BB, Type *type,
                                   Value *arraySize) {
  unsigned typeSize = getTypeSize(TD, type);

  return CallInst::CreateMalloc(BB, intPtrTy, type,
                                ConstantInt::get(intPtrTy, typeSize),
                                arraySize);
}

static Constant *getNonameGlobal(LLVMContext &C, Module &M) {
  Constant *noname = ConstantArray::get(C, "noname");
  GlobalVariable *noname_var =
        new GlobalVariable(M, noname->getType(), true,
                           GlobalValue::PrivateLinkage, noname, "noname_str");
  noname_var->setUnnamedAddr(true);
  noname_var->setAlignment(1);

  std::vector<Value *> params;
  params.push_back(ConstantInt::get(TypeBuilder<types::i<32>, true>::get(C), 0));
  params.push_back(ConstantInt::get(TypeBuilder<types::i<32>, true>::get(C), 0));

  return ConstantExpr::getInBoundsGetElementPtr(noname_var, params);
}

Instruction *Kleerer::call_klee_make_symbolic(Function *klee_make_symbolic,
                                              Constant *noname, BasicBlock *BB,
                                              Type *type, Value *addr,
                                              Value *arraySize) {
  std::vector<Value *> p;

  if (addr->getType() != voidPtrType)
    addr = new BitCastInst(addr, voidPtrType, "", BB);
  p.push_back(addr);
  Value *size = ConstantInt::get(uintType, getTypeSize(TD, type));
  if (arraySize)
    size = BinaryOperator::CreateMul(arraySize, size,
                                     "make_symbolic_size", BB);
  p.push_back(size);
  p.push_back(noname);

  check(klee_make_symbolic, p);

  return CallInst::Create(klee_make_symbolic, p);
}

void Kleerer::makeAiStateSymbolic(Function *klee_make_symbolic, Module &M,
                                  BasicBlock *BB, Constant *noname) {
  Constant *zero = ConstantInt::get(intType, 0);
  GlobalVariable *ai_state = M.getGlobalVariable("__ai_state", true);
/*      new GlobalVariable(M, intType, false, GlobalValue::ExternLinkage,
                         ConstantInt::get(intType, 0), "__ai_state");*/
  ai_state->setInitializer(zero);
  BB->getInstList().push_back(call_klee_make_symbolic(klee_make_symbolic,
                                                      noname, BB, intType,
                                                      ai_state));
  new StoreInst(zero, ai_state, "", true, BB);
}

Constant *Kleerer::get_assert_fail()
{
  Type *constCharPtrTy = TypeBuilder<const char *, false>::get(C);
  AttrListPtr attrs;
  attrs = attrs.addAttr(~0, Attribute::NoReturn);
  return M.getOrInsertFunction("__assert_fail", attrs, Type::getVoidTy(C),
                               constCharPtrTy, constCharPtrTy, uintType,
                               constCharPtrTy, NULL);
}

BasicBlock *Kleerer::checkAiState(Function *mainFun, BasicBlock *BB,
                                  Constant *noname) {
  Constant *zero = ConstantInt::get(intType, 0);
  GlobalVariable *ai_state = M.getGlobalVariable("__ai_state", true);

  BasicBlock *finalBB = BasicBlock::Create(C, "final", mainFun);
  BasicBlock *assBB = BasicBlock::Create(C, "assertBB", mainFun);
  std::vector<Value *> params;
  params.push_back(noname);
  params.push_back(noname);
  params.push_back(zero);
  params.push_back(noname);
  CallInst::Create(get_assert_fail(), params, "", assBB);
  new UnreachableInst(C, assBB);

  Value *ai_stateVal = new LoadInst(ai_state, "", true, BB);
  Value *ai_stateIsZero = new ICmpInst(*BB, CmpInst::ICMP_EQ, ai_stateVal, zero);
  BranchInst::Create(finalBB, assBB, ai_stateIsZero, BB);

  return finalBB;
}

void Kleerer::addGlobals(Module &mainMod) {
  for (Module::global_iterator I = M.global_begin(), E = M.global_end();
       I != E; ++I) {
    GlobalVariable &G = *I;
    if (!G.isDeclaration() || G.hasInitializer())
      continue;
//    errs() << "glob: " << G.getName() << '\n';
/*    GlobalValue::LinkageTypes linkage = G.getLinkage();
    if (linkage == GlobalValue::ExternalWeakLinkage)
      linkage = GlobalValue::CommonLinkage;
    new GlobalVariable(mainMod, G.getType(), G.isConstant(), linkage,
                       Constant::getNullValue(G.getType()), G.getName());*/
    Constant *xxx = Constant::getNullValue(G.getType()->getElementType());
/*    errs() << "xxx=";
    xxx->getType()->print(errs());
    errs() << "\nyyy=";
    G.getType()->getElementType()->print(errs());
    errs() << "\n";*/
    G.setInitializer(xxx);
  }
}

void Kleerer::writeMain(Function &F) {
  std::string name = M.getModuleIdentifier() + ".main." + F.getNameStr() + ".o";
//  Module mainMod(name, C);
/*  Function *callie = Function::Create(F.getFunctionType(),
                    GlobalValue::ExternalLinkage, F.getName(), &mainMod);*/
  Function *mainFun = Function::Create(TypeBuilder<int(), false>::get(C),
                    GlobalValue::ExternalLinkage, "main", &M);
  BasicBlock *mainBB = BasicBlock::Create(C, "entry", mainFun);
  BasicBlock::InstListType &insList = mainBB->getInstList();

  Function *klee_make_symbolic = Function::Create(
              TypeBuilder<void(void *, unsigned, const char *), false>::get(C),
              GlobalValue::ExternalLinkage, "klee_make_symbolic", &M);
/*  Function *klee_int = Function::Create(
              TypeBuilder<int(const char *), false>::get(C),
              GlobalValue::ExternalLinkage, "klee_int", &M);*/

  Constant *noname = getNonameGlobal(C, M);
  std::vector<Value *> noname_vec;
  noname_vec.push_back(noname);
//  F.dump();

  std::vector<Value *> params;
  for (Function::const_arg_iterator I = F.arg_begin(), E = F.arg_end(); I != E;
       ++I) {
    const Value &param = *I;
    Type *type = param.getType();
#ifdef DEBUG_WRITE_MAIN
    errs() << "param\n  ";
    param.print(errs());
    errs() << "\n  type=";
    type->print(errs());
    errs() << "\n";
#endif
    Value *val = NULL;
    Instruction *ins;
    if (const PointerType *PT = dyn_cast<const PointerType>(type)) {
//      insList.push_back(ins = CallInst::Create(klee_int, noname_vec));
      Value *arrSize = ConstantInt::get(intType, 4000);
      insList.push_back(ins = createMalloc(mainBB, PT->getElementType(),
                                           arrSize));
      insList.push_back(call_klee_make_symbolic(klee_make_symbolic, noname,
                                                mainBB, PT->getElementType(),
                                                ins, arrSize));
      bool cast = false;
      if (ins->getType() != voidPtrType) {
        insList.push_back(ins = new BitCastInst(ins, voidPtrType));
        cast = true;
      }
      ins = GetElementPtrInst::CreateInBounds(ins,
               ConstantInt::get(TypeBuilder<types::i<64>, true>::get(C), 2000));
      insList.push_back(ins);
      if (cast)
        insList.push_back(ins = new BitCastInst(ins, type));
      val = ins;
    } else if (IntegerType *IT = dyn_cast<IntegerType>(type)) {
      insList.push_front(ins = new AllocaInst(IT));
      insList.push_back(call_klee_make_symbolic(klee_make_symbolic, noname,
                                                mainBB, type, ins));
      insList.push_back(ins = new LoadInst(ins));
      val = ins;
    }
    if (val)
      params.push_back(val);
  }
//  mainFun->viewCFG();

  makeAiStateSymbolic(klee_make_symbolic, M, mainBB, noname);
  addGlobals(M);
#ifdef DEBUG_WRITE_MAIN
  errs() << "==============\n";
  errs() << mainMod;
  errs() << "==============\n";
#endif
  check(&F, params);

  CallInst::Create(&F, params, "", mainBB);
  BasicBlock *final = checkAiState(mainFun, mainBB, noname);
  ReturnInst::Create(C, ConstantInt::get(mainFun->getReturnType(), 0),
                     final);

#ifdef DEBUG_WRITE_MAIN
  mainFun->viewCFG();
#endif

  std::string ErrorInfo;
  raw_fd_ostream out(name.c_str(), ErrorInfo);
  if (!ErrorInfo.empty()) {
    errs() << __func__ << ": cannot write '" << name << "'!\n";
    return;
  }

//  errs() << mainMod;

  PassManager Passes;
  Passes.add(createVerifierPass());
  Passes.run(M);

  WriteBitcodeToFile(&M, out);
  errs() << __func__ << ": written: '" << name << "'\n";
  mainFun->eraseFromParent();
  klee_make_symbolic->eraseFromParent();
//  done = true;
}

void Kleerer::handleFun(Function &F) {
/*  for (Function::const_iterator I = F.begin(), E = F.end(); I != E; ++I)
    handleBB()*/
  bool handle = false;
  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I) {
    int ret = handleIns(*I);
    if (ret < 0)
      return;
    handle |= !!ret;
  }

  if (handle)
    writeMain(F);
}

bool Kleerer::run() {
  for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
    Function &F = *I;
    if (F.isDeclaration())
      continue;
/*    if (!F.getName().equals("tty_reset_termios"))
      continue;*/
    handleFun(F);
    if (done)
      break;
  }
  return false;
}

bool KleererPass::runOnModule(Module &M) {
  TargetData &TD = getAnalysis<TargetData>();
  Kleerer K(*this, M, TD);
  return K.run();
}
