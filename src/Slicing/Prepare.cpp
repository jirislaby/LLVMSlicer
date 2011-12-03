//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include <assert.h>
#include <cstring>

#include "llvm/Constants.h"
#include "llvm/Function.h"
#include "llvm/GlobalVariable.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TypeBuilder.h"
#include "llvm/Type.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;

namespace {
  class Prepare : public ModulePass {
    public:
      static char ID;

      Prepare() : ModulePass(ID) {}

      virtual bool runOnModule(Module &M);

    private:
      static void replaceInsLoad(llvm::Function &F, llvm::CallInst *CI);
      static void replaceInsStore(llvm::Function &F, llvm::CallInst *CI);
      static bool runOnFunction(Function &F);
  };
}

static RegisterPass<Prepare> X("prepare", "Prepares the code for slicing");
char Prepare::ID;

static GlobalVariable *getAiVar(Function &F, const CallInst *CI) {
  const ConstantExpr *GEP =
    dyn_cast<const ConstantExpr>(CI->getOperand(0));
  assert(GEP && GEP->getOpcode() == Instruction::GetElementPtr);
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
  glob->setInitializer(ConstantInt::get(
                            TypeBuilder<int, false>::get(F.getContext()), 0));
  return glob;
}

void Prepare::replaceInsLoad(Function &F, CallInst *CI) {
  GlobalVariable *glob = getAiVar(F, CI);
  LoadInst *LI = new LoadInst(glob, 0, true);
  LI->setDebugLoc(CI->getDebugLoc());
  ReplaceInstWithInst(CI, LI);
}

void Prepare::replaceInsStore(Function &F, CallInst *CI) {
  GlobalVariable *glob = getAiVar(F, CI);
  StoreInst *SI = new StoreInst(CI->getOperand(1), glob, true);
  SI->setDebugLoc(CI->getDebugLoc());
  ReplaceInstWithInst(CI, SI);
}

bool Prepare::runOnFunction(Function &F) {
  bool modified = false;
  const Module *M = F.getParent();
  const Function *__ai_load = M->getFunction("__ai_load");
  const Function *__ai_store = M->getFunction("__ai_store");

  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E;) {
    Instruction *ins = &*I;
    ++I;
    if (CallInst *CI = dyn_cast<CallInst>(ins)) {
      Function *callee = CI->getCalledFunction();
      if (callee) {
        if (callee == __ai_load) {
          replaceInsLoad(F, CI);
          modified = true;
        } else if (callee == __ai_store) {
          replaceInsStore(F, CI);
          modified = true;
        }
      }
    }
  }
  return modified;
}

bool Prepare::runOnModule(Module &M) {
  for (llvm::Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
    Function &F = *I;
    if (!F.isDeclaration())
      runOnFunction(F);
  }
  return true;
}
