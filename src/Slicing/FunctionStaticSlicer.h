// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef SLICING_FUNCTIONSTATICSLICER_H
#define SLICING_FUNCTIONSTATICSLICER_H

#include <map>
#include <utility> /* pair */

#include "llvm/Value.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/InstIterator.h"

#include "../PointsTo/PointsTo.h"
#include "../Modifies/Modifies.h"
#include "PostDominanceFrontier.h"

namespace llvm { namespace slicing {

typedef llvm::SmallSetVector<llvm::ptr::PointsToSets::Pointee, 10> ValSet;

class InsInfo {
private:
  typedef llvm::ptr::PointsToSets::Pointee Pointee;

public:
  InsInfo(const llvm::Instruction *i, const llvm::ptr::PointsToSets &PS,
                   const llvm::mods::Modifies &MOD);

  const Instruction *getIns() const { return ins; }

  bool addRC(const Pointee &var) { return RC.insert(var); }
  bool addDEF(const Pointee &var) { return DEF.insert(var); }
  bool addREF(const Pointee &var) { return REF.insert(var); }
  void deslice() { sliced = false; }

  ValSet::const_iterator RC_begin() const { return RC.begin(); }
  ValSet::const_iterator RC_end() const { return RC.end(); }
  ValSet::const_iterator DEF_begin() const { return DEF.begin(); }
  ValSet::const_iterator DEF_end() const { return DEF.end(); }
  ValSet::const_iterator REF_begin() const { return REF.begin(); }
  ValSet::const_iterator REF_end() const { return REF.end(); }

  bool isSliced() const { return sliced; }

private:
  void addDEFArray(const ptr::PointsToSets &PS, const Value *V,
      uint64_t lenConst);
  void addREFArray(const ptr::PointsToSets &PS, const Value *V,
      uint64_t lenConst);
  void handleVariousFuns(const ptr::PointsToSets &PS, const CallInst *C,
      const Function *F);

  const llvm::Instruction *ins;
  ValSet RC, DEF, REF;
  bool sliced;
};

class FunctionStaticSlicer {
  typedef llvm::ptr::PointsToSets::Pointee Pointee;

public:
  typedef std::map<const llvm::Instruction *, InsInfo *> InsInfoMap;

  FunctionStaticSlicer(llvm::Function &F, llvm::ModulePass *MP,
                       const llvm::ptr::PointsToSets &PT,
		       const llvm::mods::Modifies &mods) :
	  fun(F), MP(MP) {
    for (llvm::inst_iterator I = llvm::inst_begin(F), E = llvm::inst_end(F);
	 I != E; ++I)
      insInfoMap.insert(InsInfoMap::value_type(&*I, new InsInfo(&*I, PT, mods)));
  }
  ~FunctionStaticSlicer();

  ValSet::const_iterator relevant_begin(const llvm::Instruction *I) const {
    return getInsInfo(I)->RC_begin();
  }
  ValSet::const_iterator relevant_end(const llvm::Instruction *I) const {
    return getInsInfo(I)->RC_end();
  }

  ValSet::const_iterator REF_begin(const llvm::Instruction *I) const {
    return getInsInfo(I)->REF_begin();
  }
  ValSet::const_iterator REF_end(const llvm::Instruction *I) const {
    return getInsInfo(I)->REF_end();
  }

  template<typename FwdValueIterator>
  bool addCriterion(const llvm::Instruction *ins, FwdValueIterator b,
		    FwdValueIterator const e, bool desliceIfChanged = false) {
    InsInfo *ii = getInsInfo(ins);
    bool change = false;
    for (; b != e; ++b)
      if (ii->addRC(*b))
        change = true;
    if (change && desliceIfChanged)
      ii->deslice();
    return change;
  }

  void addInitialCriterion(const llvm::Instruction *ins,
			   const Pointee &cond = Pointee(0, 0),
			   bool deslice = true) {
    InsInfo *ii = getInsInfo(ins);
    if (cond.first)
      ii->addRC(cond);
    ii->deslice();
  }
  void calculateStaticSlice();
  bool slice();
  static void removeUndefs(ModulePass *MP, Function &F);

  void addSkipAssert(const llvm::CallInst *CI) {
    skipAssert.insert(CI);
  }

  bool shouldSkipAssert(const llvm::CallInst *CI) {
    return skipAssert.count(CI);
  }

private:
  llvm::Function &fun;
  llvm::ModulePass *MP;
  InsInfoMap insInfoMap;
  llvm::SmallSetVector<const llvm::CallInst *, 10> skipAssert;

  static bool sameValues(const Pointee &val1, const Pointee &val2);
  void crawlBasicBlock(const llvm::BasicBlock *bb);
  bool computeRCi(InsInfo *insInfoi, InsInfo *insInfoj);
  bool computeRCi(InsInfo *insInfoi);
  void computeRC();

  void computeSCi(const llvm::Instruction *i, const llvm::Instruction *j);
  void computeSC();

  bool computeBC();
  bool updateRCSC(llvm::PostDominanceFrontier::DomSetType::const_iterator start,
                  llvm::PostDominanceFrontier::DomSetType::const_iterator end);

  void dump();

  InsInfo *getInsInfo(const llvm::Instruction *i) const {
    InsInfoMap::const_iterator I = insInfoMap.find(i);
    assert(I != insInfoMap.end());
    return I->second;
  }

  static void removeUndefBranches(ModulePass *MP, Function &F);
  static void removeUndefCalls(ModulePass *MP, Function &F);
};

bool findInitialCriterion(llvm::Function &F, FunctionStaticSlicer &ss,
                          bool startingFunction = false);

}}

#endif
