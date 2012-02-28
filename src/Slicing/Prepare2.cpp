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

using namespace llvm;

namespace {
  class Prepare : public ModulePass {
    public:
      static char ID;

      Prepare() : ModulePass(ID) {}

      virtual bool runOnModule(Module &M);

    private:
      static void makeNop(Function *F);
      static void deleteAsmBodies(Module &M);
  };
}

static RegisterPass<Prepare> X("prepare2", "Prepares the code for slicing");
char Prepare::ID;

void Prepare::makeNop(Function *F) {
  F->deleteBody();
  BasicBlock *BB = BasicBlock::Create(F->getContext(), "entry", F);
  ReturnInst::Create(F->getContext(), F->getReturnType()->isVoidTy() ? NULL :
                     Constant::getNullValue(F->getReturnType()), BB);
  F->setLinkage(GlobalValue::InternalLinkage);
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

bool Prepare::runOnModule(Module &M) {
  deleteAsmBodies(M);

  return true;
}
