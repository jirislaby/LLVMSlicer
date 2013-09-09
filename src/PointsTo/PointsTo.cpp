// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include <map>

#include "llvm/BasicBlock.h"
#include "llvm/DataLayout.h"
#include "llvm/Support/GetElementPtrTypeIterator.h"
#include "llvm/Instruction.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"

#include "PointsTo.h"
#include "RuleExpressions.h"

#include "../Languages/LLVM.h"

namespace llvm { namespace ptr { namespace detail {

class CallMaps {
private:
  /* return type -> function */
  typedef std::multimap<const Type *, const Function *> FunctionsMap;
  typedef std::multimap<const Type *, const CallInst *> CallsMap;

public:
  CallMaps(const Module &M) {
    buildCallMaps(M);
  }

  template <typename OutIterator>
  void collectCallRuleCodes(const CallInst *c, const Function *f,
      OutIterator out);

  template <typename OutIterator>
  void collectCallRuleCodes(const CallInst *c, OutIterator out);

  template <typename OutIterator>
  void collectReturnRuleCodes(const ReturnInst *r, OutIterator out);

private:
  FunctionsMap FM;
  CallsMap CM;

  static bool compatibleTypes(const Type *t1, const Type *t2);
  static bool compatibleFunTypes(const FunctionType *f1,
      const FunctionType *f2);
  static RuleCode argPassRuleCode(const Value *l, const Value *r);
  void buildCallMaps(const Module &M);
};

RuleCode CallMaps::argPassRuleCode(const Value *l, const Value *r)
{
    if (isa<ConstantPointerNull const>(r))
	return ruleCode(ruleVar(l) = ruleNull(r));
    if (hasExtraReference(l))
	if (hasExtraReference(r))
	    return ruleCode(ruleVar(l) = ruleVar(r));
	else
	    return ruleCode(ruleVar(l) = *ruleVar(r));
    else
	if (hasExtraReference(r))
	    return ruleCode(ruleVar(l) = &ruleVar(r));
	else
	    return ruleCode(ruleVar(l) = ruleVar(r));
}

template <typename OutIterator>
void CallMaps::collectCallRuleCodes(const CallInst *c, const Function *f,
    OutIterator out) {
  assert(!isInlineAssembly(c) && "Inline assembly is not supported!");

  if (memoryManStuff(f) && !isMemoryAllocation(f))
    return;

  if (isMemoryAllocation(f)) {
    const Value *V = c;
    *out++ = ruleCode(ruleVar(V) = ruleAllocSite(V));
  } else {
    Function::const_arg_iterator fit = f->arg_begin();

    for (size_t i = 0; fit != f->arg_end(); ++fit, ++i)
      if (isPointerValue(&*fit))
	*out++ = argPassRuleCode(&*fit, elimConstExpr(c->getOperand(i)));
  }
}

bool CallMaps::compatibleTypes(const Type *t1, const Type *t2) {

  /*
   * Casting sucks, we can call (int *) with (char *) parameters.
   * Let's over-approximate.
   */
  if (t1->isPointerTy() && t2->isPointerTy())
    return true;

  return t1 == t2;
}

bool CallMaps::compatibleFunTypes(const FunctionType *f1,
		const FunctionType *f2) {

  unsigned params1 = f1->getNumParams();
  unsigned params2 = f2->getNumParams();

  if (!f1->isVarArg() && !f2->isVarArg() && params1 != params2)
    return false;

  if (!compatibleTypes(f1->getReturnType(), f2->getReturnType()))
    return false;

  for (int i = 0; i < params1 && i < params2; i++)
    if (!compatibleTypes(f1->getParamType(i), f2->getParamType(i)))
      return false;

  return true;
}

template<typename OutIterator>
void CallMaps::collectCallRuleCodes(const CallInst *c, OutIterator out) {

    if (const Function *f = c->getCalledFunction()) {
      collectCallRuleCodes(c, f, out);
      return;
    }

    const FunctionType *funTy = getCalleePrototype(c);
    const Type *retTy = funTy->getReturnType();

    for (FunctionsMap::const_iterator I = FM.lower_bound(retTy),
	E = FM.upper_bound(retTy); I != E; ++I) {
      const Function *fun = I->second;

      if (compatibleFunTypes(funTy, fun->getFunctionType()))
	collectCallRuleCodes(c, fun, out);
    }
}

template<typename OutIterator>
void CallMaps::collectReturnRuleCodes(const ReturnInst *r, OutIterator out) {
  const Value *retVal = r->getReturnValue();

  if (!retVal || !isPointerValue(retVal))
    return;

  const Function *f = r->getParent()->getParent();
  const FunctionType *funTy = f->getFunctionType();
  const Type *retTy = funTy->getReturnType();

  for (CallsMap::const_iterator b = CM.lower_bound(retTy),
      e = CM.upper_bound(retTy); b != e; ++b) {
    const CallInst *CI = b->second;

    if (const Function *g = CI->getCalledFunction()) {
      if (f == g)
	*out++ = argPassRuleCode(CI, retVal);
    } else if (compatibleFunTypes(funTy, getCalleePrototype(CI)))
	*out++ = argPassRuleCode(CI, retVal);
  }
}

void CallMaps::buildCallMaps(const Module &M) {
    for (Module::const_iterator f = M.begin(); f != M.end(); ++f) {
	if (!f->isDeclaration()) {
	    const FunctionType *funTy = f->getFunctionType();

	    FM.insert(std::make_pair(funTy->getReturnType(), &*f));
	}

	for (const_inst_iterator i = inst_begin(f), E = inst_end(f);
		i != E; ++i) {
	    if (const CallInst *CI = dyn_cast<CallInst>(&*i)) {
		if (!isInlineAssembly(CI) && !callToMemoryManStuff(CI)) {
		    const FunctionType *funTy = getCalleePrototype(CI);

		    CM.insert(std::make_pair(funTy->getReturnType(), CI));
		}
	    } else if (const StoreInst *SI = dyn_cast<StoreInst>(&*i)) {
		const Value *r = SI->getValueOperand();

		if (hasExtraReference(r) && memoryManStuff(r)) {
		    const Function *fn = dyn_cast<Function>(r);
		    const FunctionType *funTy = fn->getFunctionType();

		    FM.insert(std::make_pair(funTy->getReturnType(), fn));
		}
	    }
	}
    }
}

}}}

namespace llvm { namespace ptr {

typedef PointsToSets::PointsToSet PTSet;
typedef PointsToSets::Pointer Ptr;

static bool applyRule(PointsToSets &S, ASSIGNMENT<
		    VARIABLE<const llvm::Value *>,
		    VARIABLE<const llvm::Value *>
		    > const& E) {
    const llvm::Value *lval = E.getArgument1().getArgument();
    const llvm::Value *rval = E.getArgument2().getArgument();
    PTSet &L = S[Ptr(lval, -1)];
    const PTSet &R = S[Ptr(rval, -1)];
    const std::size_t old_size = L.size();

    std::copy(R.begin(), R.end(), std::inserter(L, L.end()));

    return old_size != L.size();
}

static int64_t accumulateConstantOffset(const GetElementPtrInst *gep,
	const DataLayout &DL, bool &isArray) {
    int64_t off = 0;

    for (gep_type_iterator GTI = gep_type_begin(gep), GTE = gep_type_end(gep);
	    GTI != GTE; ++GTI) {
	ConstantInt *OpC = dyn_cast<ConstantInt>(GTI.getOperand());
	if (!OpC) /* skip non-const array indices */
	    continue;
	if (OpC->isZero())
	    continue;

	int64_t ElementIdx = OpC->getSExtValue();

	// Handle a struct index, which adds its field offset to the pointer.
	if (StructType *STy = dyn_cast<StructType>(*GTI)) {
	    const StructLayout *SL = DL.getStructLayout(STy);
	    off += SL->getElementOffset(ElementIdx);
	    continue;
	} else if (SequentialType *STy = dyn_cast<SequentialType>(*GTI)) {
	    off += ElementIdx * DL.getTypeStoreSize(GTI.getIndexedType());
	    isArray = true;
	    continue;
	}
#ifdef FIELD_DEBUG
	errs() << "skipping " << OpC->getValue() << " in ";
	gep->dump();
#endif
    }

    return off;
}

static bool checkOffset(const DataLayout &DL, const Value *Rval, uint64_t sum) {
  if (const GlobalVariable *GV = dyn_cast<GlobalVariable>(Rval)) {
    if (GV->hasInitializer() &&
	sum >= DL.getTypeAllocSize(GV->getInitializer()->getType()))
      return false;
  } else if (const AllocaInst *AI = dyn_cast<AllocaInst>(Rval)) {
    if (!AI->isArrayAllocation() &&
	sum >= DL.getTypeAllocSize(AI->getAllocatedType()))
      return false;
  }

  return true;
}

static bool applyRule(PointsToSets &S, const llvm::DataLayout &DL, ASSIGNMENT<
		    VARIABLE<const llvm::Value *>,
		    GEP<VARIABLE<const llvm::Value *> >
		    > const& E) {
    const llvm::Value *lval = E.getArgument1().getArgument();
    const llvm::Value *rval = E.getArgument2().getArgument().getArgument();
    PTSet &L = S[Ptr(lval, -1)];
    const std::size_t old_size = L.size();

    const GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(rval);
    const llvm::Value *op = elimConstExpr(gep->getPointerOperand());
    bool isArray = false;
    int64_t off = accumulateConstantOffset(gep, DL, isArray);

    if (hasExtraReference(op)) {
	L.insert(Ptr(op, off)); /* VAR = REF */
    } else {
	const PTSet &R = S[Ptr(op, -1)];
	for (PTSet::const_iterator I = R.begin(), E = R.end(); I != E; ++I) {
	    assert(I->second >= 0);

	    /* disable recursive structures */
	    if (L.count(*I))
		    continue;

	    const Value *Rval = I->first;

	    if (off && (isa<Function>(Rval) || isa<ConstantPointerNull>(Rval)))
	      continue;

	    int64_t sum = I->second + off;

	    if (!checkOffset(DL, Rval, sum))
	      continue;

	    if (sum < 0) {
		    assert(I->second >= 0);
#ifdef DEBUG_CROPPING
		    errs() << "variable index, cropping to 0: " <<
			    I->second << "+" << off << "\n\t";
		    gep->dump();
		    errs() << "\tPTR=";
		    Rval->dump();
#endif
		    sum = 0;
	    }

	    /* an unsoundness :) */
	    if (isArray && sum > 64)
		sum = 64;

	    L.insert(Ptr(Rval, sum)); /* V = V */
	}
    }

    return old_size != L.size();
}

static bool applyRule(PointsToSets &S, ASSIGNMENT<
		    VARIABLE<const llvm::Value *>,
		    REFERENCE<VARIABLE<const llvm::Value *> >
		    > const& E) {
    const llvm::Value *lval = E.getArgument1().getArgument();
    const llvm::Value *rval = E.getArgument2().getArgument().getArgument();
    PTSet &L = S[Ptr(lval, -1)];
    const std::size_t old_size = L.size();

    L.insert(Ptr(rval, 0));

    return old_size != L.size();
}

static bool applyRule(PointsToSets &S, ASSIGNMENT<
		    VARIABLE<const llvm::Value *>,
		    DEREFERENCE< VARIABLE<const llvm::Value *> >
		    > const& E, const int idx = -1)
{
    const llvm::Value *lval = E.getArgument1().getArgument();
    const llvm::Value *rval = E.getArgument2().getArgument().getArgument();
    PTSet &L = S[Ptr(lval, idx)];
    PTSet &R = S[Ptr(rval, -1)];
    const std::size_t old_size = L.size();

    for (PTSet::const_iterator i = R.begin(); i!=R.end(); ++i) {
	PTSet &X = S[*i];
	std::copy(X.begin(), X.end(), std::inserter(L, L.end()));
    }

    return old_size != L.size();
}

static bool applyRule(PointsToSets &S, ASSIGNMENT<
		    DEREFERENCE<VARIABLE<const llvm::Value *> >,
		    VARIABLE<const llvm::Value *>
		    > const& E)
{
    const llvm::Value *lval = E.getArgument1().getArgument().getArgument();
    const llvm::Value *rval = E.getArgument2().getArgument();
    PTSet &L = S[Ptr(lval, -1)];
    PTSet &R = S[Ptr(rval, -1)];
    bool change = false;

    for (PTSet::const_iterator i = L.begin(); i != L.end(); ++i) {
	PTSet &X = S[*i];
	const std::size_t old_size = X.size();

	std::copy(R.begin(), R.end(), std::inserter(X, X.end()));
	change = change || X.size() != old_size;
    }

    return change;
}

static bool applyRule(PointsToSets &S, ASSIGNMENT<
		    DEREFERENCE<VARIABLE<const llvm::Value *> >,
		    REFERENCE<VARIABLE<const llvm::Value *> >
		    > const &E)
{
    const llvm::Value *lval = E.getArgument1().getArgument().getArgument();
    const llvm::Value *rval = E.getArgument2().getArgument().getArgument();
    PTSet &L = S[Ptr(lval, -1)];
    bool change = false;

    for (PTSet::const_iterator i = L.begin(); i != L.end(); ++i) {
	PTSet &X = S[*i];
	const std::size_t old_size = X.size();

	X.insert(Ptr(rval, 0));
	change = change || X.size() != old_size;
    }

    return change;
}

static bool applyRule(PointsToSets &S, ASSIGNMENT<
		    DEREFERENCE<VARIABLE<const llvm::Value *> >,
		    DEREFERENCE<VARIABLE<const llvm::Value *> >
		    > const& E)
{
    const llvm::Value *lval = E.getArgument1().getArgument().getArgument();
    const llvm::Value *rval = E.getArgument2().getArgument().getArgument();
    PTSet &L = S[Ptr(lval, -1)];
    bool change = false;

    for (PTSet::const_iterator i = L.begin(); i != L.end(); ++i)
	if (applyRule(S, (ruleVar(i->first) = *ruleVar(rval)).getSort(),
				i->second))
	    change = true;

    return change;
}

static bool applyRule(PointsToSets &S, ASSIGNMENT<
		    VARIABLE<const llvm::Value *>,
		    ALLOC<const llvm::Value *>
		    > const &E)
{
    const llvm::Value *lval = E.getArgument1().getArgument();
    const llvm::Value *rval = E.getArgument2().getArgument();
    PTSet &L = S[Ptr(lval, -1)];
    const std::size_t old_size = L.size();

    L.insert(Ptr(rval, 0));

    return old_size != L.size();
}

static bool applyRule(PointsToSets &S, ASSIGNMENT<
		    VARIABLE<const llvm::Value *>,
		    NULLPTR<const llvm::Value *>
		    > const &E)
{
    const llvm::Value *lval = E.getArgument1().getArgument();
    const llvm::Value *rval = E.getArgument2().getArgument();
    PTSet &L = S[Ptr(lval, -1)];
    const std::size_t old_size = L.size();

    L.insert(Ptr(rval, 0));

    return old_size != L.size();
}

static bool applyRule(PointsToSets &S, ASSIGNMENT<
		    DEREFERENCE<VARIABLE<const llvm::Value *> >,
		    NULLPTR<const llvm::Value *>
		    > const &E)
{
    const llvm::Value *lval = E.getArgument1().getArgument().getArgument();
    const llvm::Value *rval = E.getArgument2().getArgument();
    PTSet &L = S[Ptr(lval, -1)];
    bool change = false;

    for (PTSet::const_iterator i = L.begin(); i != L.end(); ++i) {
	PTSet &X = S[*i];
	const std::size_t old_size = X.size();

	X.insert(Ptr(rval, 0));
	change = change || X.size() != old_size;
    }

    return change;
}

static bool applyRule(PointsToSets &S, DEALLOC<const llvm::Value *>) {
    return false;
}

static bool applyRules(const RuleCode &RC, PointsToSets &S,
		const llvm::DataLayout &DL)
{
    const llvm::Value *lval = RC.getLvalue();
    const llvm::Value *rval = RC.getRvalue();

    switch (RC.getType()) {
    case RCT_VAR_ASGN_ALLOC:
	return applyRule(S, (ruleVar(lval) = ruleAllocSite(rval)).getSort());
    case RCT_VAR_ASGN_NULL:
	return applyRule(S, (ruleVar(lval) = ruleNull(rval)).getSort());
    case RCT_VAR_ASGN_VAR:
	return applyRule(S, (ruleVar(lval) = ruleVar(rval)).getSort());
    case RCT_VAR_ASGN_GEP:
	return applyRule(S, DL,
			(ruleVar(lval) = ruleVar(rval).gep()).getSort());
    case RCT_VAR_ASGN_REF_VAR:
	return applyRule(S, (ruleVar(lval) = &ruleVar(rval)).getSort());
    case RCT_VAR_ASGN_DREF_VAR:
	return applyRule(S, (ruleVar(lval) = *ruleVar(rval)).getSort());
    case RCT_DREF_VAR_ASGN_NULL:
	return applyRule(S, (*ruleVar(lval) = ruleNull(rval)).getSort());
    case RCT_DREF_VAR_ASGN_VAR:
	return applyRule(S, (*ruleVar(lval) = ruleVar(rval)).getSort());
    case RCT_DREF_VAR_ASGN_REF_VAR:
	return applyRule(S, (*ruleVar(lval) = &ruleVar(rval)).getSort());
    case RCT_DREF_VAR_ASGN_DREF_VAR:
	return applyRule(S, (*ruleVar(lval) = *ruleVar(rval)).getSort());
    case RCT_DEALLOC:
	return applyRule(S, ruleDeallocSite(RC.getValue()).getSort());
    default:
	assert(0);
    }
}

/*
 * It does not really work -- it prunes too much. Like it does not take into
 * account bitcast instructions in the code.
 */
static PointsToSets &pruneByType(PointsToSets &S) {
  typedef PointsToSets::mapped_type PTSet;
  for (PointsToSets::iterator s = S.begin(); s != S.end(); ) {
      const llvm::Value *first = s->first.first;
      if (llvm::isa<llvm::Function>(first)) {
	const PointsToSets::iterator tmp = s++;
	S.getContainer().erase(tmp);
      } else {
#if 0
	if (isPointerValue(first)) {
	  const llvm::Type *firstTy;
	  if (const llvm::BitCastInst *BC =
		      llvm::dyn_cast<llvm::BitCastInst>(first))
	    firstTy = getPointedType(BC->getSrcTy());
	  else
	    firstTy = getPointedType(first);

	  for (typename PTSet::const_iterator v = s->second.begin();
	       v != s->second.end(); ) {
	    const llvm::Value *second = *v;
	    const llvm::Type *secondTy = second->getType();

	    if (hasExtraReference(second))
		    secondTy = llvm::cast<llvm::PointerType>(secondTy)->
			    getElementType();
	    if (const llvm::ArrayType *AT =
			    llvm::dyn_cast<llvm::ArrayType>(secondTy))
		    secondTy = AT->getElementType();

	    if (firstTy != secondTy) {
	      typename PTSet::iterator const tmp = v++;
	      s->second.erase(tmp);
	    } else
	      ++v;
	  }
	}
#endif
	++s;
      }
  }
  return S;
}

static PointsToSets &fixpoint(const ProgramStructure &P, PointsToSets &S)
{
  bool change;

  DataLayout DL(&P.getModule());

  do {
    change = false;

    for (ProgramStructure::const_iterator i = P.begin(); i != P.end(); ++i)
      change |= applyRules(*i, S, DL);
  } while (change);

  return S;
}

PointsToSets &computePointsToSets(const ProgramStructure &P, PointsToSets &S) {
  return pruneByType(fixpoint(P, S));
}

const PTSet &
getPointsToSet(const llvm::Value *const &memLoc, const PointsToSets &S,
		const int idx) {
  const PointsToSets::const_iterator it = S.find(Ptr(memLoc, idx));
  if (it == S.end()) {
    static const PTSet emptySet;
    errs() << "WARNING[PointsTo]: No points-to set has been found: ";
    memLoc->print(errs());
    errs() << '\n';
    return emptySet;
  }
  return it->second;
}

ProgramStructure::ProgramStructure(Module &M) : M(M) {
    for (Module::const_global_iterator g = M.global_begin(), E = M.global_end();
	    g != E; ++g)
      if (isGlobalPointerInitialization(&*g))
	detail::toRuleCode(&*g,std::back_inserter(this->getContainer()));

    detail::CallMaps CM(M);

    for (Module::const_iterator f = M.begin(); f != M.end(); ++f) {
	for (const_inst_iterator i = inst_begin(f), E = inst_end(f);
		i != E; ++i) {
	    if (isPointerManipulation(&*i))
		detail::toRuleCode(&*i,
			    std::back_inserter(this->getContainer()));
	    else if (const CallInst *c = dyn_cast<CallInst>(&*i)) {
		if (!isInlineAssembly(c))
		    CM.collectCallRuleCodes(c,
			std::back_inserter(this->getContainer()));
	    } else if (const ReturnInst *r = dyn_cast<ReturnInst>(&*i)) {
		CM.collectReturnRuleCodes(r,
			std::back_inserter(this->getContainer()));
	    }
	}
    }
#ifdef PS_DEBUG
    errs() << "==PS START\n";
    for (const_iterator I = getContainer().begin(), E = getContainer().end();
	    I != E; ++I) {
	const RuleCode &rc = *I;
	errs() << "\tTYPE=" << rc.getType() << "\n\tL=";
	rc.getLvalue()->dump();
	errs() << "\tR=";
	rc.getRvalue()->dump();
    }
    errs() << "==PS END\n";
#endif
}

}}
