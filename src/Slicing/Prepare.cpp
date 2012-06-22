//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include <assert.h>
#include <cstring>

#include "llvm/BasicBlock.h"
#include "llvm/Constants.h"
#include "llvm/Function.h"
#include "llvm/GlobalVariable.h"
#include "llvm/InlineAsm.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TypeBuilder.h"
#include "llvm/Type.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "../Callgraph/Callgraph.h"
#include "../PointsTo/AlgoAndersen.h"
#include "../PointsTo/PointsTo.h"

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
      static bool handleAsm(Function &F, CallInst *CI);
      static void makeNop(Function *F);
      static void deleteAsmBodies(Module &M);
      static bool runOnFunction(Function &F);

      template<typename PointsToSets>
      void findInitFuns(Module &M, const PointsToSets &PS);
  };
}

static RegisterPass<Prepare> X("prepare", "Prepares the code for slicing");
char Prepare::ID;

static GlobalVariable *getAiVar(Function &F, const CallInst *CI) {
  const ConstantExpr *GEP =
    dyn_cast<const ConstantExpr>(CI->getOperand(1));
  assert(GEP && GEP->getOpcode() == Instruction::GetElementPtr);
  const GlobalVariable *strVar =
    dyn_cast<const GlobalVariable>(GEP->getOperand(0));
  assert(strVar && strVar->hasInitializer());
  const ConstantDataArray *str =
    dyn_cast<const ConstantDataArray>(strVar->getInitializer());
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
  StoreInst *SI = new StoreInst(CI->getOperand(2), glob, true);
  SI->setDebugLoc(CI->getDebugLoc());
  ReplaceInstWithInst(CI, SI);
}

bool Prepare::handleAsm(Function &F, CallInst *CI) {
  const InlineAsm *IA = cast<InlineAsm>(CI->getCalledValue());
  std::string ASM = IA->getAsmString();
  std::string CONS = IA->getConstraintString();

//  BasicBlock *BB = CI->getParent();

  if ((ASM.empty() && !CI->getNumArgOperands()) || /* a barrier */
      !ASM.compare(0, 6, "1:\tud2") ||
      !ASM.compare("lfence") || !ASM.compare("mfence") ||
      !ASM.compare("sfence")) {
/*    errs() << ASM << " (" << F.getName() << "): " << ASM.empty() << " " << !ASM.compare(0, 6, "1:\tud2") << " " <<
      !ASM.compare("lfence") << " " << !ASM.compare("mfence") << " " <<
      !ASM.compare("sfence");
    BB->dump();*/
    CI->eraseFromParent();
    return true;
  } else if (ASM.empty() && CI->getNumArgOperands() == 1) { /* reloc hide */
    ReplaceInstWithInst(CI, CastInst::CreatePointerCast(CI->getArgOperand(0),
                                                        CI->getType()));
    return true;
  } else if (!ASM.compare("movs %gs:${1:c},$0") || /* reading pda */
             !ASM.compare("movl %gs:${1:c},$0") ||
             !ASM.compare("movq %gs:${1:c},$0")) {
    const ConstantInt *param = dyn_cast<ConstantInt>(CI->getArgOperand(0));
    if (param) {
      const APInt &paramVal = param->getValue();
      Module *M = F.getParent();
      if (paramVal == 0) { /* current */
        ReplaceInstWithInst(CI, new LoadInst(
                                  M->getOrInsertGlobal("__ai_current_singleton",
                                                       CI->getType())));
        return true;
      } else { /* others, let's fake it with global var */
        GlobalVariable *GV = dyn_cast<GlobalVariable>(
              M->getOrInsertGlobal("__ai_pda_" + paramVal.toString(10, false),
                                   CI->getType()));
        GV->setInitializer(Constant::getNullValue(CI->getType()));
        if (CI->getType()->isPointerTy())
          errs() << "Warn ptr type => we set it to point to NULL\n";
        ReplaceInstWithInst(CI, new LoadInst(GV));
        return true;
      }
    }
  } else if (!ASM.compare(0, 16, "call __put_user_") ||
             !ASM.compare(0, 16, "call __get_user_") ) {
    BasicBlock::iterator it(CI);
    ReplaceInstWithValue(CI->getParent()->getInstList(), it,
                         Constant::getNullValue(CI->getType()));
    return true;
  }
  errs() << "ASM str (" << F.getName() << "): " << ASM << " from:\n";
  CI->dump();
  errs() << "===========\n";
  return false;
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
      if (CI->isInlineAsm()) {
        modified |= handleAsm(F, CI);
        continue;
      }
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

void Prepare::makeNop(Function *F) {
  F->deleteBody();
  BasicBlock *BB = BasicBlock::Create(F->getContext(), "entry", F);
  ReturnInst::Create(F->getContext(), F->getReturnType()->isVoidTy() ? NULL :
                     Constant::getNullValue(F->getReturnType()), BB);
}

#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))

void Prepare::deleteAsmBodies(llvm::Module &M) {
  static const char *toDelete[] = {
    "atomic_inc", "atomic_dec", "atomic_add", "atomic_sub",
    "atomic_dec_and_test", "atomic_add_return",
    "atomic64_inc", "atomic64_dec", "atomic64_add", "atomic64_sub",
    "local_inc", "local_dec",
    "__fswab16", "__fswab32", "__fswab64",
    "__xchg", "__cmpxchg",
    "__set_bit", "__clear_bit", "set_bit", "clear_bit",
    "variable_test_bit",
    "__test_and_set_bit", "__test_and_clear_bit",
    "test_and_set_bit", "test_and_clear_bit",
    "__fls", "fls", "__ffs", "ffs", "ffz",

    "___arch__swab32", "___arch__swab64" // generates false positives in stats
  };
  static const char *_makeNop[] = {
    "pagefault_disable",
    "__raw_local_save_flags", "raw_local_irq_restore",
    "raw_local_irq_enable", "raw_local_irq_disable",
    "__raw_spin_is_contended",
    "local_bh_enable", "local_bh_disable",
    "schedule", "schedule_timeout", "schedule_timeout_interruptible",
    "schedule_timeout_uninterruptible",
    "preempt_schedule",
    "msleep", "msleep_interruptible", "__udelay", "__const_udelay",
    "printk_ratelimit", "warn_slowpath", "warn_on_slowpath", "dump_stack",
    "printk", "vprintk", "snd_verbose_printk",
    "rep_nop",
    "inb", "inw", "inl",
    "insb", "insw", "insl",
    "outb", "outw", "outl",
    "outsb", "outsw", "outsl",
    "inb_p", "inw_p", "inl_p",
    "outb_p", "outw_p", "outl_p",
    "readb", "readw", "readl", "readq",
    "writeb", "writew", "writel", "writeq",
    "__readb", "__readw", "__readl", "__readq",
    "__writeb", "__writew", "__writel", "__writeq",

    "mod_timer", "__mod_timer", "del_timer", "del_timer_sync",
    "complete", "wait_for_completion",
    "interruptible_sleep_on",
    "add_wait_queue", "remove_wait_queue", "prepare_to_wait", "finish_wait",
    "__tasklet_schedule",
    "queue_work", "schedule_work", "flush_scheduled_work",
    "schedule_delayed_work",
    "__wake_up", "wake_up_process", "wake_up_state", "kill_fasync"
  };
  unsigned int i;

  for (i = 0; i < ARRAY_SIZE(toDelete); i++) {
    Function *F = M.getFunction(toDelete[i]);
    if (F)
      F->deleteBody();
  }
  for (i = 0; i < ARRAY_SIZE(_makeNop); i++) {
    Function *F = M.getFunction(_makeNop[i]);
    if (F)
      makeNop(F);
  }
}

template<typename PointsToSets>
void Prepare::findInitFuns(Module &M, const PointsToSets &PS) {
  callgraph::Callgraph CG(M, PS);

  SmallVector<Constant *, 10> initFns;
  Type *ETy = TypeBuilder<void *, false>::get(M.getContext());

  for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
    Function &F = *I;
    if (F.isDeclaration())
      continue;
    callgraph::Callgraph::range_iterator callees = CG.callees(&F);
    if (std::distance(callees.first, callees.second))
      continue;
    initFns.push_back(ConstantExpr::getBitCast(&F, ETy));
  }
  ArrayType *ATy = ArrayType::get(ETy, initFns.size());
  new GlobalVariable(M, ATy, true, GlobalVariable::InternalLinkage,
                     ConstantArray::get(ATy, initFns),
                     "__ai_init_functions");
}

bool Prepare::runOnModule(Module &M) {
  ptr::PointsToSets<ptr::ANDERSEN>::Type PS;
  {
    ptr::ProgramStructure P(M);
    computePointsToSets(P, PS);
  }

  deleteAsmBodies(M);

  for (llvm::Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
    Function &F = *I;
    if (!F.isDeclaration())
      runOnFunction(F);
  }

  findInitFuns(M, PS);

  return true;
}
