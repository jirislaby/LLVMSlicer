#include "llvm/Attributes.h"
#include "llvm/Constants.h"
#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/PassManager.h"
#include "llvm/Module.h"
#include "llvm/DataLayout.h"
#include "llvm/TypeBuilder.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/raw_ostream.h"

#include "Callgraph/Callgraph.h"
#include "PointsTo/AlgoAndersen.h"
#include "PointsTo/PointsTo.h"
#include "Slicing/Prepare.h"

using namespace llvm;

namespace {
  class KleererPass : public ModulePass {
  public:
    static char ID;

    KleererPass() : ModulePass(ID) { }

    virtual bool runOnModule(Module &M);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      AU.addRequired<DataLayout>();
    }
  };
}

class Kleerer {
public:
  Kleerer(ModulePass &modPass, Module &M, DataLayout &TD,
          callgraph::Callgraph &CG) : modPass(modPass),
      M(M), TD(TD), CG(CG), C(M.getContext()), intPtrTy(TD.getIntPtrType(C)),
      done(false) {
    voidPtrType = TypeBuilder<void *, false>::get(C);
    intType = TypeBuilder<int, false>::get(C);
    uintType = TypeBuilder<unsigned, false>::get(C);
  }

  bool run();

private:
  ModulePass &modPass;
  Module &M;
  DataLayout &TD;
  callgraph::Callgraph &CG;
  LLVMContext &C;
  IntegerType *intPtrTy;
  bool done;
  Function *klee_make_symbolic;

  /* types */
  Type *voidPtrType;
  Type *intType;
  Type *uintType;

  Value *handlePtrArg(BasicBlock *mainBB, Constant *name, PointerType *PT);
  void prepareArguments(Function &F, BasicBlock *mainBB,
                        std::vector<Value *> &params);
  void writeMain(Function &F);

  Constant *get_assert_fail();

  Instruction *createMalloc(BasicBlock *BB, Type *type, unsigned typeSize,
                            Value *arraySize);
  Instruction *call_klee_make_symbolic(Constant *name, BasicBlock *BB,
                                       Type *type, Value *addr,
                                       Value *arraySize = 0);
  Instruction *mallocSymbolic(BasicBlock *BB, Constant *name, Type *elemTy,
                              unsigned typeSize, Value *arrSize);
  void makeGlobalsSymbolic(Module &M, BasicBlock *BB);
  BasicBlock *checkAiState(Function *mainFun, BasicBlock *BB,
                           const DebugLoc &debugLoc);
  void addGlobals(Module &M);
};

static RegisterPass<KleererPass> X("kleerer", "Prepares a module for Klee");
char KleererPass::ID;

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

static unsigned getTypeSize(DataLayout &TD, Type *type) {
  if (type->isFunctionTy()) /* it is not sized, weird */
    return TD.getPointerSize();

  if (!type->isSized())
    return 100; /* FIXME */

  if (StructType *ST = dyn_cast<StructType>(type))
    return TD.getStructLayout(ST)->getSizeInBytes();

  return TD.getTypeAllocSize(type);
}

Instruction *Kleerer::createMalloc(BasicBlock *BB, Type *type,
                                   unsigned typeSize, Value *arraySize) {
  return CallInst::CreateMalloc(BB, intPtrTy, type,
                                ConstantInt::get(intPtrTy, typeSize),
                                arraySize);
}

static Constant *getGlobalString(LLVMContext &C, Module &M,
                                 const StringRef &str) {
  Constant *strArray = ConstantDataArray::getString(C, str);
  GlobalVariable *strVar =
        new GlobalVariable(M, strArray->getType(), true,
                           GlobalValue::PrivateLinkage, strArray, "");
  strVar->setUnnamedAddr(true);
  strVar->setAlignment(1);

  std::vector<Value *> params;
  params.push_back(ConstantInt::get(TypeBuilder<types::i<32>, true>::get(C), 0));
  params.push_back(ConstantInt::get(TypeBuilder<types::i<32>, true>::get(C), 0));

  return ConstantExpr::getInBoundsGetElementPtr(strVar, params);
}

Instruction *Kleerer::call_klee_make_symbolic(Constant *name, BasicBlock *BB,
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
  p.push_back(name);

  check(klee_make_symbolic, p);

  return CallInst::Create(klee_make_symbolic, p);
}

/*
 * it also initializes __ai_state_*
 */
void Kleerer::makeGlobalsSymbolic(Module &M, BasicBlock *BB) {
  Constant *zero = ConstantInt::get(intType, 0);
  for (Module::global_iterator I = M.global_begin(), E = M.global_end();
      I != E; ++I) {
    GlobalVariable &GV = *I;
    if (GV.isConstant() || !GV.hasName())
	continue;
    StringRef GVName = GV.getName();
    if (GVName.startswith("llvm."))
	    continue;
    if (GVName.startswith("__ai_") && !GVName.startswith("__ai_state_"))
	    continue;
/*    errs() << "TU " << GVName << " ";
    GV.getType()->getElementType()->dump();
    errs() << "\n\t" << GVName << "\n";*/
    Constant *glob_str = getGlobalString(C, M, GVName);
    BB->getInstList().push_back(call_klee_make_symbolic(glob_str, BB,
		GV.getType()->getElementType(), &GV));
    if (GVName.startswith("__ai_state_"))
	new StoreInst(zero, &GV, "", true, BB);
  }
}

Constant *Kleerer::get_assert_fail()
{
  Type *constCharPtrTy = TypeBuilder<const char *, false>::get(C);
  AttrListPtr attrs = AttrListPtr().addAttr(C, ~0,
		  Attributes::get(C, Attributes::NoReturn));
  return M.getOrInsertFunction("__assert_fail", attrs, Type::getVoidTy(C),
                               constCharPtrTy, constCharPtrTy, uintType,
                               constCharPtrTy, NULL);
}

BasicBlock *Kleerer::checkAiState(Function *mainFun, BasicBlock *BB,
                                  const DebugLoc &debugLoc) {
  Module *M = mainFun->getParent();
  Constant *zero = ConstantInt::get(intType, 0);

  BasicBlock *finalBB = BasicBlock::Create(C, "final", mainFun);
  BasicBlock *assBB = BasicBlock::Create(C, "assertBB", mainFun);
  std::vector<Value *> params;
  params.push_back(getGlobalString(C, *M, "leaving function with lock held"));
  params.push_back(getGlobalString(C, *M, "n/a"));
  params.push_back(zero);
  params.push_back(getGlobalString(C, *M, "main"));
  CallInst::Create(get_assert_fail(), params, "", assBB)->setDebugLoc(debugLoc);
  new UnreachableInst(C, assBB);
  Value *sum = zero;

  for (Module::global_iterator I = M->global_begin(), E = M->global_end();
      I != E; ++I) {
    GlobalVariable &ai_state = *I;
    if (!ai_state.hasName() || !ai_state.getName().startswith("__ai_state_"))
      continue;
    Value *ai_stateVal = new LoadInst(&ai_state, "", true, BB);
    sum = BinaryOperator::Create(BinaryOperator::Add, ai_stateVal, sum, "", BB);
  }

  Value *ai_stateIsZero = new ICmpInst(*BB, CmpInst::ICMP_EQ, sum, zero);
  BranchInst::Create(finalBB, assBB, ai_stateIsZero, BB);

  return finalBB;
}

void Kleerer::addGlobals(Module &mainMod) {
  for (Module::global_iterator I = M.global_begin(), E = M.global_end();
       I != E; ++I) {
    GlobalVariable &G = *I;
    if (!G.isDeclaration() || G.hasInitializer())
      continue;
    Constant *xxx = Constant::getNullValue(G.getType()->getElementType());
    G.setInitializer(xxx);
  }
}

struct st_desc {
  unsigned long flag;
#define STF_ONE  1
};

static const struct st_desc *getStDesc(const Type *elemTy) {
  typedef std::map<std::string, struct st_desc> StructMap;

  static const StructMap::value_type structMapData[] = {
    StructMap::value_type("pci_dev", (struct st_desc){ STF_ONE }),
  };

  #define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))

  static const StructMap structMap(structMapData,
                                   structMapData + ARRAY_SIZE(structMapData));

  if (const StructType *ST = dyn_cast<StructType>(elemTy))
    if (!ST->isLiteral() && ST->hasName()) {

      /* each struct name has "struct." prepended */
      StructMap::const_iterator I = structMap.find(ST->getName().substr(7));
      if (I != structMap.end())
        return &I->second;
    }

  return NULL;
}

Instruction *Kleerer::mallocSymbolic(BasicBlock *BB, Constant *name,
                                     Type *elemTy, unsigned typeSize,
                                     Value *arrSize) {
  BasicBlock::InstListType &insList = BB->getInstList();
  Instruction *ins = createMalloc(BB, elemTy, typeSize, arrSize);
  insList.push_back(ins);
  insList.push_back(call_klee_make_symbolic(name, BB, elemTy, ins, arrSize));
  return ins;
}

Value *Kleerer::handlePtrArg(BasicBlock *mainBB, Constant *name,
                             PointerType *PT) {
  BasicBlock::InstListType &insList = mainBB->getInstList();
  Instruction *ins;
  Type *elemTy = PT->getElementType();
  const struct st_desc *st_desc = getStDesc(elemTy);
  unsigned typeSize = getTypeSize(TD, elemTy);
  Value *arrSize = NULL;
  if (!st_desc || !(st_desc->flag & STF_ONE))
    arrSize = ConstantInt::get(intType, typeSize < (1 << 20) / 4192 ? 4192 :
                               (1 << 20) / typeSize);

  ins = mallocSymbolic(mainBB, name, elemTy, typeSize, arrSize);
  if (arrSize) {
    bool cast = ins->getType() != voidPtrType;
    if (cast)
      insList.push_back(ins = new BitCastInst(ins, voidPtrType));
    ins = GetElementPtrInst::CreateInBounds(ins,
           ConstantInt::get(TypeBuilder<types::i<64>, true>::get(C), 2048));
    insList.push_back(ins);
    if (cast)
      insList.push_back(ins = new BitCastInst(ins, PT));
  }

  return ins;
}

void Kleerer::prepareArguments(Function &F, BasicBlock *mainBB,
                               std::vector<Value *> &params) {
  BasicBlock::InstListType &insList = mainBB->getInstList();

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
    Constant *name = getGlobalString(C, M, param.hasName() ? param.getName() :
                                     "noname");
    if (PointerType *PT = dyn_cast<PointerType>(type)) {
      val = handlePtrArg(mainBB, name, PT);
    } else if (IntegerType *IT = dyn_cast<IntegerType>(type)) {
      Instruction *ins;
      insList.push_front(ins = new AllocaInst(IT));
      insList.push_back(call_klee_make_symbolic(name, mainBB, type, ins));
      insList.push_back(ins = new LoadInst(ins));
      val = ins;
    }
    if (val)
      params.push_back(val);
  }
}

void Kleerer::writeMain(Function &F) {
  std::string name = M.getModuleIdentifier() + ".main." + F.getName().str() + ".o";
  Function *mainFun = Function::Create(TypeBuilder<int(), false>::get(C),
                    GlobalValue::ExternalLinkage, "main", &M);
  BasicBlock *mainBB = BasicBlock::Create(C, "entry", mainFun);

  klee_make_symbolic = Function::Create(
              TypeBuilder<void(void *, unsigned, const char *), false>::get(C),
              GlobalValue::ExternalLinkage, "klee_make_symbolic", &M);
/*  Function *klee_int = Function::Create(
              TypeBuilder<int(const char *), false>::get(C),
              GlobalValue::ExternalLinkage, "klee_int", &M);*/

//  F.dump();

  std::vector<Value *> params;
  prepareArguments(F, mainBB, params);
//  mainFun->viewCFG();

  makeGlobalsSymbolic(M, mainBB);
  addGlobals(M);
#ifdef DEBUG_WRITE_MAIN
  errs() << "==============\n";
  errs() << mainMod;
  errs() << "==============\n";
#endif
  check(&F, params);

  CallInst::Create(&F, params, "", mainBB);
  BasicBlock *final = checkAiState(mainFun, mainBB, F.back().back().getDebugLoc());
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
  klee_make_symbolic = NULL;
//  done = true;
}

bool Kleerer::run() {
  Function *F__assert_fail = M.getFunction("__assert_fail");
  if (!F__assert_fail) /* nothing to find here bro */
    return false;

  callgraph::Callgraph::range_iterator RI = CG.callees(F__assert_fail);
  if (std::distance(RI.first, RI.second) == 0)
    return false;

  const ConstantArray *initFuns = getInitFuns(M);
  assert(initFuns && "No initial functions found. Did you run -prepare?");

  for (ConstantArray::const_op_iterator I = initFuns->op_begin(),
       E = initFuns->op_end(); I != E; ++I) {
    const ConstantExpr *CE = cast<ConstantExpr>(&*I);
    assert(CE->getOpcode() == Instruction::BitCast);
    Function &F = *cast<Function>(CE->getOperand(0));

    callgraph::Callgraph::const_iterator II, EE;
    llvm::tie(II, EE) = CG.calls(&F);
    for (; II != EE; ++II) {
      const Function *callee = (*II).second;
      if (callee == F__assert_fail) {
        writeMain(F);
        break;
      }
    }
    if (done)
      break;
  }
  return false;
}

bool KleererPass::runOnModule(Module &M) {
  DataLayout &TD = getAnalysis<DataLayout>();
  ptr::PointsToSets<ptr::ANDERSEN>::Type PS;
  {
    ptr::ProgramStructure P(M);
    computePointsToSets(P, PS);
  }

  callgraph::Callgraph CG(M, PS);

  Kleerer K(*this, M, TD, CG);
  return K.run();
}
