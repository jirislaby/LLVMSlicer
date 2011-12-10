// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef SLICING_FUNCTIONSTATICSLICER_H
#define SLICING_FUNCTIONSTATICSLICER_H

#include <map>

#include "llvm/Value.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/InstIterator.h"

#include "PostDominanceFrontier.h"

namespace llvm { namespace slicing {

typedef llvm::SmallSetVector<const llvm::Value *, 10> ValSet;

class InsInfo {
public:
  template<typename PointsToSets, typename ModifiesSets>
  InsInfo(const llvm::Instruction *i, PointsToSets const& PS,
                   ModifiesSets const& MOD);

  const Instruction *getIns() const { return ins; }

  bool addRC(const llvm::Value *var) { return RC.insert(var); }
  bool addDEF(const llvm::Value *var) { return DEF.insert(var); }
  bool addREF(const llvm::Value *var) { return REF.insert(var); }
  void deslice() { sliced = false; }

  ValSet::const_iterator RC_begin() const { return RC.begin(); }
  ValSet::const_iterator RC_end() const { return RC.end(); }
  ValSet::const_iterator DEF_begin() const { return DEF.begin(); }
  ValSet::const_iterator DEF_end() const { return DEF.end(); }
  ValSet::const_iterator REF_begin() const { return REF.begin(); }
  ValSet::const_iterator REF_end() const { return REF.end(); }

  bool isSliced() const { return sliced; }

private:
  const llvm::Instruction *ins;
  ValSet RC, DEF, REF;
  bool sliced;
};

class FunctionStaticSlicer {
public:
  typedef std::map<const llvm::Instruction *, InsInfo *> InsInfoMap;

  template<typename PointsToSets, typename ModifiesSets>
  FunctionStaticSlicer(llvm::Function &F, llvm::ModulePass *MP,
                       PointsToSets &PT, ModifiesSets &mods) : fun(F), MP(MP) {
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

  template<typename FwdValueIterator>
  bool addCriterion(const llvm::Instruction *ins, FwdValueIterator b,
		    FwdValueIterator const e, bool deslice = false) {
    InsInfo *ii = getInsInfo(ins);
    bool change = false;
    for (; b != e; ++b)
      if (ii->addRC(*b))
        change = true;
    if (deslice)
      ii->deslice();
    return change;
  }

  void addInitialCriterion(const llvm::Instruction *ins,
			   const llvm::Value *cond = 0, bool deslice = true) {
    InsInfo *ii = getInsInfo(ins);
    if (cond)
      ii->addRC(cond);
    ii->deslice();
  }
  void calculateStaticSlice();
  bool slice();
  static void removeUndefBranches(ModulePass *MP, Function &F);

private:
  llvm::Function &fun;
  llvm::ModulePass *MP;
  InsInfoMap insInfoMap;

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
    return insInfoMap.find(i)->second;
  }
};

bool findInitialCriterion(llvm::Function &F, FunctionStaticSlicer &ss,
                          bool startingFunction = false);

}}

#endif
