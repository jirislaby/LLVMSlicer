// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include <functional>

#include "llvm/BasicBlock.h"
#include "llvm/Instruction.h"
#include "llvm/Module.h"

#include "PointsTo.h"
#include "RuleExpressions.h"

namespace llvm { namespace ptr {

struct RuleFunction {
    typedef std::function<bool(PointsToSets &)> Type;

    static inline bool identity(PointsToSets) { return false; }
};

class Rules {
public:
    typedef std::vector<RuleFunction::Type> RuleFunctions;
    typedef typename RuleFunctions::const_iterator const_iterator;

    template<typename Sort>
    void insert(RuleExpression<Sort> const& E)
    {
	rules.push_back( getRuleFunction(E.getSort()) );
    }

    const_iterator begin() const { return rules.begin(); }
    const_iterator end() const { return rules.end(); }

private:
    RuleFunctions rules;
};

static RuleFunction::Type getRuleFunction(ASSIGNMENT<
		    VARIABLE<const llvm::Value *>,
		    VARIABLE<const llvm::Value *>
		    > const& E) {
    struct local {
	static bool function(PointsToSets &S,
			     const llvm::Value *lval,
			     const llvm::Value *rval) {
	    typedef PointsToSets::PointsToSet PointsToSet;
	    PointsToSet& L = S[lval];
	    PointsToSet const& R = S[rval];
	    std::size_t const old_size = L.size();
	    std::copy(R.begin(),R.end(),std::inserter(L,L.end()));
	    return old_size != L.size();
	}
    };
    using std::bind;
    using std::placeholders::_1;
    return bind(&local::function,_1,
		E.getArgument1().getArgument(),
		E.getArgument2().getArgument());
}

static RuleFunction::Type getRuleFunction(ASSIGNMENT<
		    VARIABLE<const llvm::Value *>,
		    REFERENCE<
			VARIABLE<const llvm::Value *> >
		    > const& E) {
    struct local
    {
	static bool function(PointsToSets &S,
			     const llvm::Value *lval,
			     const llvm::Value *rval) {
	    PointsToSets::PointsToSet& L = S[lval];
	    std::size_t const old_size = L.size();
	    L.insert(rval);
	    return old_size != L.size();
	}
    };
    using std::bind;
    using std::placeholders::_1;
    return bind(&local::function,_1,
		E.getArgument1().getArgument(),
		E.getArgument2().getArgument().getArgument());
}

static RuleFunction::Type getRuleFunction(ASSIGNMENT<
		    VARIABLE<const llvm::Value *>,
		    DEREFERENCE< VARIABLE<const llvm::Value *> >
		    > const& E)
{
    struct local
    {
	static bool function(PointsToSets &S,
			     const llvm::Value *lval,
			     const llvm::Value *rval) {
	    typedef PointsToSets::PointsToSet PointsToSet;
	    PointsToSet& L = S[lval];
	    PointsToSet& R = S[rval];
	    std::size_t const old_size = L.size();
	    for (PointsToSet::const_iterator i = R.begin(); i!=R.end(); ++i) {
		PointsToSet& X = S[*i];
		std::copy(X.begin(),X.end(),std::inserter(L,L.end()));
	    }
	    return old_size != L.size();
	}
    };
    using std::bind;
    using std::placeholders::_1;
    return bind(&local::function,_1,
		E.getArgument1().getArgument(),
		E.getArgument2().getArgument().getArgument());
}

static RuleFunction::Type getRuleFunction(ASSIGNMENT<
		    DEREFERENCE< VARIABLE<const llvm::Value *> >,
		    VARIABLE<const llvm::Value *>
		    > const& E)
{
    struct local
    {
	static bool function(PointsToSets &S,
			     const llvm::Value *lval,
			     const llvm::Value *rval) {
	    typedef PointsToSets::PointsToSet PointsToSet;
	    PointsToSet& L = S[lval];
	    PointsToSet& R = S[rval];
	    bool change = false;
	    for (PointsToSet::const_iterator i = L.begin(); i!=L.end(); ++i) {
		PointsToSet& X = S[*i];
		std::size_t const old_size = X.size();
		std::copy(R.begin(),R.end(),std::inserter(X,X.end()));
		change = change || X.size() != old_size;
	    }
	    return change;
	}
    };
    using std::bind;
    using std::placeholders::_1;
    return bind(&local::function,_1,
		E.getArgument1().getArgument().getArgument(),
		E.getArgument2().getArgument());
}

static RuleFunction::Type getRuleFunction(ASSIGNMENT<
		    DEREFERENCE<
			VARIABLE<const llvm::Value *> >,
		    REFERENCE<
			VARIABLE<const llvm::Value *> >
		    > const &E)
{
    struct local
    {
	static bool function(PointsToSets &S,
			     const llvm::Value *lval,
			     const llvm::Value *rval
			     )
	{
	    typedef PointsToSets::PointsToSet PointsToSet;
	    PointsToSet& L = S[lval];
	    bool change = false;
	    for (PointsToSet::const_iterator i = L.begin(); i!=L.end(); ++i) {
		PointsToSet& X = S[*i];
		std::size_t const old_size = X.size();
		X.insert(rval);
		change = change || X.size() != old_size;
	    }
	    return change;
	}
    };
    using std::bind;
    using std::placeholders::_1;
    return bind(&local::function,_1,
		E.getArgument1().getArgument().getArgument(),
		E.getArgument2().getArgument().getArgument());
}

static RuleFunction::Type getRuleFunction(ASSIGNMENT<
		    DEREFERENCE<
			VARIABLE<const llvm::Value *> >,
		    DEREFERENCE<
			VARIABLE<const llvm::Value *> >
		    > const& E)
{
    struct local {
	static bool function(PointsToSets &S,
			     const llvm::Value *lval,
			     const llvm::Value *rval) {
	    typedef PointsToSets::PointsToSet PointsToSet;
	    PointsToSet& L = S[lval];
	    bool change = false;
	    for (PointsToSet::const_iterator i = L.begin(); i!=L.end(); ++i)
		if (getRuleFunction(
			(ruleVar(*i) = *ruleVar(rval)).getSort())
			(S))
		    change = true;
	    return change;
	}
    };
    using std::bind;
    using std::placeholders::_1;
    return bind(&local::function,_1,
		E.getArgument1().getArgument().getArgument(),
		E.getArgument2().getArgument().getArgument());
}

static RuleFunction::Type getRuleFunction(ASSIGNMENT<
		    VARIABLE<const llvm::Value *>,
		    ALLOC<const llvm::Value *>
		    > const &E)
{
    struct local
    {
	static bool function(PointsToSets &S,
			     const llvm::Value *lval,
			     const llvm::Value *rval) {
	    PointsToSets::PointsToSet& L = S[lval];
	    std::size_t const old_size = L.size();
	    L.insert(rval);
	    return old_size != L.size();
	}
    };
    using std::bind;
    using std::placeholders::_1;
    return bind(&local::function,_1,
		E.getArgument1().getArgument(),
		E.getArgument2().getArgument());
}

static RuleFunction::Type getRuleFunction(ASSIGNMENT<
		    VARIABLE<const llvm::Value *>,
		    NULLPTR<const llvm::Value *>
		    > const &E)
{
    struct local
    {
	static bool function(PointsToSets &S,
			     const llvm::Value *lval,
			     const llvm::Value *rval) {
	    PointsToSets::PointsToSet &L = S[lval];
	    std::size_t const old_size = L.size();
	    L.insert(rval);
	    return old_size != L.size();
	}
    };
    using std::bind;
    using std::placeholders::_1;
    return bind(&local::function,_1,
		E.getArgument1().getArgument(),
		E.getArgument2().getArgument());
}

static RuleFunction::Type getRuleFunction(ASSIGNMENT<
		    DEREFERENCE<
			VARIABLE<const llvm::Value *> >,
		    NULLPTR<const llvm::Value *>
		    > const &E)
{
    struct local
    {
	static bool function(PointsToSets &S,
			     const llvm::Value *lval,
			     const llvm::Value *rval) {
	    typedef PointsToSets::PointsToSet PointsToSet;
	    PointsToSet& L = S[lval];
	    bool change = false;
	    for (PointsToSet::const_iterator i = L.begin(); i!=L.end(); ++i) {
		PointsToSet& X = S[*i];
		std::size_t const old_size = X.size();
		L.insert(rval);
		change = change || X.size() != old_size;
	    }
	    return change;
	}
    };
    using std::bind;
    using std::placeholders::_1;
    return bind(&local::function,_1,
		E.getArgument1().getArgument().getArgument(),
		E.getArgument2().getArgument());
}

static RuleFunction::Type getRuleFunction(DEALLOC<const llvm::Value *>) {
    return typename RuleFunction::Type(&RuleFunction::identity);
}

static void getRulesOfCommand(RuleCode const& RC, Rules &R)
{
    switch (RC.getType())
    {
	case RCT_VAR_ASGN_ALLOC:
	    R.insert(ruleVar(RC.getLvalue())=ruleAllocSite(RC.getRvalue()));
	    break;
	case RCT_VAR_ASGN_NULL:
	    R.insert(ruleVar(RC.getLvalue()) = ruleNull(RC.getRvalue()));
	    break;
	case RCT_VAR_ASGN_VAR:
	    R.insert(ruleVar(RC.getLvalue()) = ruleVar(RC.getRvalue()));
	    break;
	case RCT_VAR_ASGN_REF_VAR:
	    R.insert(ruleVar(RC.getLvalue()) = &ruleVar(RC.getRvalue()));
	    break;
	case RCT_VAR_ASGN_DREF_VAR:
	    R.insert(ruleVar(RC.getLvalue()) = *ruleVar(RC.getRvalue()));
	    break;
	case RCT_DREF_VAR_ASGN_NULL:
	    R.insert(*ruleVar(RC.getLvalue()) = ruleNull(RC.getRvalue()));
	    break;
	case RCT_DREF_VAR_ASGN_VAR:
	    R.insert(*ruleVar(RC.getLvalue()) = ruleVar(RC.getRvalue()));
	    break;
	case RCT_DREF_VAR_ASGN_REF_VAR:
	    R.insert(*ruleVar(RC.getLvalue()) = &ruleVar(RC.getRvalue()));
	    break;
	case RCT_DREF_VAR_ASGN_DREF_VAR:
	    R.insert(*ruleVar(RC.getLvalue()) = *ruleVar(RC.getRvalue()));
	    break;
	case RCT_DEALLOC:
	    R.insert(ruleDeallocSite(RC.getValue()));
	    break;
	default:
	    break;
    }
}

/*
 * It does not really work -- it prunes too much. Like it does not take into
 * account bitcast instructions in the code.
 */
static PointsToSets &pruneByType(PointsToSets &S) {
  typedef PointsToSets::mapped_type PTSet;
  for (PointsToSets::iterator s = S.begin(); s != S.end(); ) {
      const llvm::Value *first = s->first;
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

static bool executeRules(ProgramStructure const& P, PointsToSets &S)
{
  bool change = false;

  for (ProgramStructure::const_iterator i = P.begin(); i != P.end(); ++i) {
    Rules rules;
    getRulesOfCommand(*i, rules);
    for (Rules::const_iterator j = rules.begin(); j != rules.end(); ++j) {
      const bool modification = (*j)(S);
      change = change || modification;
    }
  }

  return change;
}

static PointsToSets &fixpoint(const ProgramStructure &P, PointsToSets &S)
{
  while (executeRules(P, S))
      ;
  return S;
}

PointsToSets &computePointsToSets(const ProgramStructure &P, PointsToSets &S) {
  return pruneByType(fixpoint(P, S));
}

const PointsToSets::PointsToSet &
getPointsToSet(const llvm::Value *const &memLoc, const PointsToSets &S) {
  const PointsToSets::const_iterator it = S.find(memLoc);
  if (it == S.end()) {
    static const PointsToSets::PointsToSet emptySet;
    errs() << "WARNING[PointsTo]: No points-to set has been found: ";
    memLoc->print(errs());
    errs() << '\n';
    return emptySet;
  }
  return it->second;
}

ProgramStructure::ProgramStructure(Module &M) : M(M) {
    typedef llvm::Module::const_global_iterator GlobalsIter;
    for (GlobalsIter g = M.global_begin(); g != M.global_end(); ++g)
      if (llvm::isGlobalPointerInitialization(&*g))
	detail::toRuleCode(&*g,std::back_inserter(this->getContainer()));

    detail::FunctionsMap FM;
    detail::CallsMap CM;
    detail::buildCallMaps(M,FM,CM);

    typedef llvm::Module::const_iterator FunctionsIter;
    for (FunctionsIter f = M.begin(); f != M.end(); ++f) {
	typedef llvm::Function::const_iterator BasicBlocksIter;
	for (BasicBlocksIter b = f->begin(); b != f->end(); ++b)
	{
	    typedef llvm::BasicBlock::const_iterator InstructionsIter;
	    for (InstructionsIter i = b->begin(); i != b->end(); ++i)
		if (llvm::isPointerManipulation(&*i))
		    detail::toRuleCode(&*i,
				std::back_inserter(this->getContainer()));
		else if (llvm::CallInst const* const c =
			    llvm::dyn_cast<llvm::CallInst>(&*i)) {
		    if (!isInlineAssembly(c))
			detail::collectCallRuleCodes(c,
			    FM.lower_bound(llvm::getCalleePrototype(c)),
			    FM.upper_bound(llvm::getCalleePrototype(c)),
			    std::back_inserter(this->getContainer()));
		} else if (llvm::ReturnInst const* const r =
			    llvm::dyn_cast<llvm::ReturnInst>(&*i)) {
		    llvm::FunctionType const* const t =
			r->getParent()->getParent()->getFunctionType();
		    detail::collectReturnRuleCodes(r,CM.lower_bound(t),
			    CM.upper_bound(t),
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
