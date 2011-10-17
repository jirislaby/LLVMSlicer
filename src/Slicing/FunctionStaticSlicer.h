#ifndef SLICING_FUNCTIONSTATICSLICER_H
#define SLICING_FUNCTIONSTATICSLICER_H

#include <map>

namespace llvm {

typedef llvm::SmallSetVector<const Value *, 10> ValSet;

class InsInfo {
public:
  template<typename PointsToSets, typename ModifiesSets>
  InsInfo(const Instruction *i, PointsToSets const& PS,
                   ModifiesSets const& MOD);

  bool addRC(const Value *var) { return RC.insert(var); }
  void deslice() { sliced = false; }

  ValSet::const_iterator RC_begin() const { return RC.begin(); }
  ValSet::const_iterator RC_end() const { return RC.end(); }
  ValSet::const_iterator DEF_begin() const { return DEF.begin(); }
  ValSet::const_iterator DEF_end() const { return DEF.end(); }
  ValSet::const_iterator REF_begin() const { return REF.begin(); }
  ValSet::const_iterator REF_end() const { return REF.end(); }

  bool isSliced() const { return sliced; }

private:
  const Instruction *ins;
  ValSet RC, DEF, REF;
  bool sliced;
};

class FunctionStaticSlicer {
public:
  typedef std::map<const Instruction *, InsInfo *> InsInfoMap;

  template<typename PointsToSets, typename ModifiesSets>
  FunctionStaticSlicer(Function &F, PostDominatorTree &PDT, PostDominanceFrontier &PDF,
               PointsToSets &PT, ModifiesSets &mods) : fun(F), PDT(PDT),
               PDF(PDF) {
    for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I)
      insInfoMap.insert(InsInfoMap::value_type(&*I, new InsInfo(&*I, PT, mods)));
  }
  ~FunctionStaticSlicer();

  void addInitialCriterion(const Instruction *ins, const Value *cond = 0) {
    InsInfo *ii = getInsInfo(ins);
    if (cond)
      ii->addRC(cond);
    ii->deslice();
  }
  void calculateStaticSlice();
  bool slice();

private:
  Function &fun;
  PostDominatorTree &PDT;
  PostDominanceFrontier &PDF;
  InsInfoMap insInfoMap;

  void crawlBasicBlock(const BasicBlock *bb);
  bool computeRCi(const Instruction *i, const Instruction *j);
  bool computeRCi(const Instruction *i);
  void computeRC();

  void computeSCi(const Instruction *i, const Instruction *j);
  void computeSC();

  bool computeBC();
  bool updateRCSC(PostDominanceFrontier::DomSetType::const_iterator start,
                  PostDominanceFrontier::DomSetType::const_iterator end);

  void dump();

  InsInfo *getInsInfo(const Instruction *i) const {
    return insInfoMap.find(i)->second;
  }
};

}

#endif
