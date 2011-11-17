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
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TypeBuilder.h"
#include "llvm/Type.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "Prepare.h"

using namespace llvm;
using namespace llvm::slicing;

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
  return glob;
}

void Prepare::replaceInsTrans(Function &F, CallInst *CI) {
  Type *intType = TypeBuilder<int, false>::get(F.getContext());
//  errs() << __func__ << ": ======\n";
  GlobalVariable *glob = getAiVar(F, CI);
  glob->setInitializer(ConstantInt::get(intType, 0));
  StoreInst *SI = new StoreInst(CI->getOperand(1), glob, true);
  SI->setDebugLoc(CI->getDebugLoc());
  ReplaceInstWithInst(CI, SI);
}

void Prepare::replaceInsCheck(Function &F, CallInst *CI) {
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

void Prepare::prepareFun(Function &F) {
//  F.dump();
  const Module *M = F.getParent();
  const Function *__ai_trans = M->getFunction("__ai_trans");
  const Function *__ai_check_eq = M->getFunction("__ai_check_eq");
  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E;) {
    Instruction *ins = &*I;
    ++I;
    if (CallInst *CI = dyn_cast<CallInst>(ins)) {
      Function *callee = CI->getCalledFunction();
      if (callee) {
        if (callee == __ai_trans)
          replaceInsTrans(F, CI);
        else if (callee == __ai_check_eq)
          replaceInsCheck(F, CI);
      }
    }
  }
//  F.dump();
}
