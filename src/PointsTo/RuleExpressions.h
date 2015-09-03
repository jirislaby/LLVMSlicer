// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_RULEEXPRESSIONS_H
#define POINTSTO_RULEEXPRESSIONS_H

#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"

#include "../Languages/LLVM.h"

namespace llvm { namespace ptr {

    struct Expression
    {
	virtual ~Expression() {}
    };

    template<typename SubExpr>
    struct RuleUnaryExpression : public Expression
    {
	typedef SubExpr SubExpression;

	explicit RuleUnaryExpression(SubExpression const sub)
	    : arg(sub)
	{}

	SubExpression getArgument() const
	{ return arg; }


	SubExpression arg;
    };

    template<typename SubExpr1, typename SubExpr2>
    struct RuleBinaryExpression : public Expression
    {
	typedef SubExpr1 SubExpression1;
	typedef SubExpr2 SubExpression2;

	explicit RuleBinaryExpression(SubExpression1 const sub1,
				      SubExpression2 const sub2)
	    : arg1(sub1)
	    , arg2(sub2)
	{}

	SubExpression1 getArgument1() const
	{ return arg1; }

	SubExpression2 getArgument2() const
	{ return arg2; }


	SubExpression1 arg1;
	SubExpression2 arg2;
    };

    template<typename MemLoc>
    struct VARIABLE : public RuleUnaryExpression<MemLoc> {
	VARIABLE(MemLoc const ml) : RuleUnaryExpression<MemLoc>(ml) {}
    };

    template<typename MemLoc>
    struct ALLOC : public RuleUnaryExpression<MemLoc> {
	ALLOC(MemLoc const ml) : RuleUnaryExpression<MemLoc>(ml) {}
    };

    template<typename MemLoc>
    struct DEALLOC : public RuleUnaryExpression<MemLoc> {
	DEALLOC(MemLoc const ml) : RuleUnaryExpression<MemLoc>(ml) {}
    };

    template<typename SubExpr>
    struct GEP : public RuleUnaryExpression<SubExpr> {
	GEP(SubExpr const sub) : RuleUnaryExpression<SubExpr>(sub) {}
    };

    template<typename MemLoc>
    struct NULLPTR : public RuleUnaryExpression<MemLoc> {
	NULLPTR(MemLoc const ml) : RuleUnaryExpression<MemLoc>(ml) {}
    };

    template<typename SubExpr>
    struct REFERENCE : public RuleUnaryExpression<SubExpr> {
	REFERENCE(SubExpr const sub) : RuleUnaryExpression<SubExpr>(sub) {}
    };

    template<typename SubExpr>
    struct DEREFERENCE : public RuleUnaryExpression<SubExpr> {
	DEREFERENCE(SubExpr const sub) : RuleUnaryExpression<SubExpr>(sub) {}
    };

    template<typename LSubExpr, typename RSubExpr>
    struct ASSIGNMENT : public RuleBinaryExpression<LSubExpr,RSubExpr> {
	ASSIGNMENT(LSubExpr const lsub, RSubExpr const rsub)
	    : RuleBinaryExpression<LSubExpr,RSubExpr>(lsub,rsub) {}
    };

    template<typename ExprSort>
    struct RuleExpression
    {
	typedef ExprSort Sort;

	explicit RuleExpression(Sort const& s)
	    : sort(s)
	{}

	Sort getSort() const
	{ return sort; }

	template<typename RSort>
	RuleExpression< ASSIGNMENT<Sort,RSort> >
	operator=(RuleExpression<RSort> const& r) const
	{ return makeAssignment(r); }

	template<typename RSort>
	RuleExpression< ASSIGNMENT<Sort,RSort> >
	operator=(RuleExpression<RSort> const& r)
	{ return makeAssignment(r); }

	RuleExpression< ASSIGNMENT<Sort,Sort> >
	operator=(RuleExpression<Sort> const& r)
	{ return makeAssignment(r); }

	RuleExpression< REFERENCE<Sort> > operator&() const
	{
	    return RuleExpression< REFERENCE<Sort> >(REFERENCE<Sort>(sort));
	}

	RuleExpression< DEREFERENCE<Sort> > operator*() const
	{
	    return RuleExpression< DEREFERENCE<Sort> >(DEREFERENCE<Sort>(sort));
	}

	RuleExpression< GEP<Sort> >gep() const
	{
	    return RuleExpression< GEP<Sort> >(GEP<Sort>(sort));
	}

    private:
	template<typename RSort>
	RuleExpression< ASSIGNMENT<Sort,RSort> >
	makeAssignment(RuleExpression<RSort> const& r)
	{
	    return RuleExpression< ASSIGNMENT<Sort,RSort> >(
			ASSIGNMENT<Sort,RSort>(sort,r.getSort()));
	}

	Sort sort;
    };

    template<typename MemLoc>
    RuleExpression< VARIABLE<MemLoc> > ruleVar(MemLoc const ml)
    {
	return RuleExpression< VARIABLE<MemLoc> >(VARIABLE<MemLoc>(ml));
    }

    template<typename MemLoc>
    RuleExpression< ALLOC<MemLoc> > ruleAllocSite(MemLoc const ml)
    {
	return RuleExpression< ALLOC<MemLoc> >(ALLOC<MemLoc>(ml));
    }

    template<typename MemLoc>
    RuleExpression< DEALLOC<MemLoc> > ruleDeallocSite(MemLoc const ml)
    {
	return RuleExpression< DEALLOC<MemLoc> >(DEALLOC<MemLoc>(ml));
    }

    template<typename MemLoc>
    RuleExpression< NULLPTR<MemLoc> > ruleNull(MemLoc const ml)
    {
	return RuleExpression< NULLPTR<MemLoc> >(NULLPTR<MemLoc>(ml));
    }

}}

namespace llvm { namespace ptr {

    enum RuleCodeType
    {
	RCT_UNKNOWN = 0,
	RCT_VAR_ASGN_ALLOC,
	RCT_VAR_ASGN_NULL,
	RCT_VAR_ASGN_VAR,
	RCT_VAR_ASGN_GEP,
	RCT_VAR_ASGN_REF_VAR,
	RCT_VAR_ASGN_DREF_VAR,
	RCT_DREF_VAR_ASGN_NULL,
	RCT_DREF_VAR_ASGN_VAR,
	RCT_DREF_VAR_ASGN_REF_VAR,
	RCT_DREF_VAR_ASGN_DREF_VAR,
	RCT_DEALLOC,
    };

    struct RuleCode
    {
	typedef const llvm::Value *MemoryLocation;

	RuleCode()
	    : type(RCT_UNKNOWN)
	{}

	RuleCode(ASSIGNMENT<VARIABLE<MemoryLocation>,
			    ALLOC<MemoryLocation> > const& E)
	    : type(RCT_VAR_ASGN_ALLOC)
	    , lvalue(E.getArgument1().getArgument())
	    , rvalue(E.getArgument2().getArgument())
	{}

	RuleCode(ASSIGNMENT<VARIABLE<MemoryLocation>,
			    NULLPTR<MemoryLocation> > const& E)
	    : type(RCT_VAR_ASGN_NULL)
	    , lvalue(E.getArgument1().getArgument())
	    , rvalue(E.getArgument2().getArgument())
	{}

	RuleCode(ASSIGNMENT<VARIABLE<MemoryLocation>,
			    VARIABLE<MemoryLocation> > const& E)
	    : type(RCT_VAR_ASGN_VAR)
	    , lvalue(E.getArgument1().getArgument())
	    , rvalue(E.getArgument2().getArgument())
	{}

	RuleCode(ASSIGNMENT<VARIABLE<MemoryLocation>,
			    GEP<VARIABLE<MemoryLocation> > > const& E)
	    : type(RCT_VAR_ASGN_GEP)
	    , lvalue(E.getArgument1().getArgument())
	    , rvalue(E.getArgument2().getArgument().getArgument())
	{}

	RuleCode(ASSIGNMENT<VARIABLE<MemoryLocation>,
			    REFERENCE<VARIABLE<MemoryLocation> > > const& E)
	    : type(RCT_VAR_ASGN_REF_VAR)
	    , lvalue(E.getArgument1().getArgument())
	    , rvalue(E.getArgument2().getArgument().getArgument())
	{}

	RuleCode(ASSIGNMENT<VARIABLE<MemoryLocation>,
			    DEREFERENCE<VARIABLE<MemoryLocation> > > const& E)
	    : type(RCT_VAR_ASGN_DREF_VAR)
	    , lvalue(E.getArgument1().getArgument())
	    , rvalue(E.getArgument2().getArgument().getArgument())
	{}

	RuleCode(ASSIGNMENT<DEREFERENCE<VARIABLE<MemoryLocation> >,
			    NULLPTR<MemoryLocation> > const& E)
	    : type(RCT_DREF_VAR_ASGN_NULL)
	    , lvalue(E.getArgument1().getArgument().getArgument())
	    , rvalue(E.getArgument2().getArgument())
	{}

	RuleCode(ASSIGNMENT<DEREFERENCE<VARIABLE<MemoryLocation> >,
			    VARIABLE<MemoryLocation> > const& E)
	    : type(RCT_DREF_VAR_ASGN_VAR)
	    , lvalue(E.getArgument1().getArgument().getArgument())
	    , rvalue(E.getArgument2().getArgument())
	{}

	RuleCode(ASSIGNMENT<DEREFERENCE<VARIABLE<MemoryLocation> >,
			    REFERENCE<VARIABLE<MemoryLocation> > > const& E)
	    : type(RCT_DREF_VAR_ASGN_REF_VAR)
	    , lvalue(E.getArgument1().getArgument().getArgument())
	    , rvalue(E.getArgument2().getArgument().getArgument())
	{}

	RuleCode(ASSIGNMENT<DEREFERENCE<VARIABLE<MemoryLocation> >,
			    DEREFERENCE<VARIABLE<MemoryLocation> > > const& E)
	    : type(RCT_DREF_VAR_ASGN_DREF_VAR)
	    , lvalue(E.getArgument1().getArgument().getArgument())
	    , rvalue(E.getArgument2().getArgument().getArgument())
	{}

	RuleCode(DEALLOC<MemoryLocation> const& E)
	    : type(RCT_DEALLOC)
	    , lvalue(E.getArgument())
	    , rvalue()
	{}

	RuleCodeType getType() const { return type; }
	MemoryLocation const& getLvalue() const { return lvalue; }
	MemoryLocation const& getRvalue() const { return rvalue; }
	MemoryLocation const& getValue() const { return getLvalue(); }

    private:
	RuleCodeType type;
	MemoryLocation lvalue;
	MemoryLocation rvalue;
    };

    template<typename ExprSort>
    RuleCode ruleCode(RuleExpression<ExprSort> const& E)
    {
	return RuleCode(E.getSort());
    }

}}

namespace llvm { namespace ptr { namespace detail {

  template<typename OutIterator>
  void toRuleCode(const Value *V, OutIterator out) {
    if (const llvm::Instruction *I = llvm::dyn_cast<llvm::Instruction>(V)) {
      if (const llvm::LoadInst *LI = llvm::dyn_cast<llvm::LoadInst>(I)) {
	const llvm::Value *op = elimConstExpr(LI->getPointerOperand());

	if (hasExtraReference(op))
	  *out++ = ruleCode(ruleVar(V) = ruleVar(op));
	else
	  *out++ = ruleCode(ruleVar(V) = *ruleVar(op));
      } else if (const llvm::StoreInst *SI =
		 llvm::dyn_cast<llvm::StoreInst>(I)) {
	const llvm::Value *l = elimConstExpr(SI->getPointerOperand());
	const llvm::Value *r = elimConstExpr(SI->getValueOperand());

	if (!hasExtraReference(l)) {
	  if (hasExtraReference(r))
	    *out++ = ruleCode(*ruleVar(l) = &ruleVar(r));
	  else {
	    if (isa<ConstantPointerNull>(r))
	      *out++ = ruleCode(*ruleVar(l) = ruleNull(r));
	    else
	      *out++ = ruleCode(*ruleVar(l) = ruleVar(r));
	  }
	} else {
	  if (hasExtraReference(r))
	    *out++ = ruleCode(ruleVar(l) = &ruleVar(r));
	  else {
	    if (isa<ConstantPointerNull>(r))
	      *out++ = ruleCode(ruleVar(l) = ruleNull(r));
	    else
	      *out++ = ruleCode(ruleVar(l) = ruleVar(r));
	  }
	}
      } else if (const llvm::BitCastInst *BCI =
		 llvm::dyn_cast<llvm::BitCastInst>(I)) {
	const llvm::Value *op = elimConstExpr(BCI->getOperand(0));

	if (hasExtraReference(op))
	  *out++ = ruleCode(ruleVar(V) = &ruleVar(op));
	else
	  *out++ = ruleCode(ruleVar(V) = ruleVar(op));
      } else if (const llvm::GetElementPtrInst *gep =
		 llvm::dyn_cast<llvm::GetElementPtrInst>(I)) {
	const llvm::Value *op = gep;

	*out++ = ruleCode(ruleVar(V) = ruleVar(op).gep());
      } else if (const llvm::CallInst *C =
		 llvm::dyn_cast<llvm::CallInst>(I)) {
	if (isInlineAssembly(C)) {
	} else if (isMemoryAllocation(C->getCalledValue()))
	  *out++ = ruleCode(ruleVar(V) = ruleAllocSite(V));
	else if (isMemoryDeallocation(C->getCalledValue()))
	  *out++ = ruleCode(ruleDeallocSite(V));
	else if (isMemoryCopy(C->getCalledValue()) ||
	    isMemoryMove(C->getCalledValue())) {
	  const llvm::Value *l = elimConstExpr(C->getArgOperand(0));
	  const llvm::Value *r = elimConstExpr(C->getArgOperand(1));

	  *out++ = ruleCode(*ruleVar(l) = *ruleVar(r));
	}
      } else if (const llvm::PHINode *PHI = llvm::dyn_cast<llvm::PHINode>(I)) {
	unsigned int i, n = PHI->getNumIncomingValues();

	for (i = 0; i < n; ++i) {
	  const llvm::Value *r = PHI->getIncomingValue(i);

	  if (llvm::isa<llvm::ConstantPointerNull>(r))
	      *out++ = ruleCode(ruleVar(V) = ruleNull(r));
	  else
	      *out++ = ruleCode(ruleVar(V) = ruleVar(r));
	}
      } else if (const llvm::ExtractValueInst *EV =
		 llvm::dyn_cast<llvm::ExtractValueInst>(I)) {
	// TODO: Instruction 'ExtractValueIns' has not been tested yet!

	const llvm::Value *op = EV->getAggregateOperand();
	assert(!hasExtraReference(op) && "Agregate operand must "
	       "be a value and not a pointer.");
	*out++ = ruleCode(ruleVar(V) = ruleVar(op));
	    } else if (const llvm::InsertValueInst *IV =
		       llvm::dyn_cast<llvm::InsertValueInst>(I)) {
	// TODO: Instruction 'InsertValueInst' has not been tested yet!

	const llvm::Value *l = IV->getAggregateOperand();
	assert(!hasExtraReference(l) && "Agregate operand must "
	       "be a value and not a pointer.");
	const llvm::Value *r = IV->getInsertedValueOperand();
	if (hasExtraReference(r))
	  *out++ = ruleCode(ruleVar(l) = &ruleVar(r));
	else {
	  if (isa<ConstantPointerNull>(r))
	    *out++ = ruleCode(ruleVar(l) = ruleNull(r));
	  else
	    *out++ = ruleCode(ruleVar(l) = ruleVar(r));
	}
      } else if (const llvm::IntToPtrInst *ITPI =
		 llvm::dyn_cast<llvm::IntToPtrInst>(I)) {
	errs() << __func__ << ": WARNING[PointsTo]: Integer ";

	if (const llvm::ConstantInt *C =
	    llvm::dyn_cast<llvm::ConstantInt>(ITPI->getOperand(0)))
	  errs() << "(" << C->getValue() << ") ";

	errs() << "converted to a pointer in '" <<
	    I->getParent()->getParent()->getName() <<
	    "' => getting unsound analysis!\n";
      } else if (const llvm::SelectInst *SEL =
		 llvm::dyn_cast<llvm::SelectInst>(I)) {
	  const llvm::Value *r1 = elimConstExpr(SEL->getTrueValue());
	  const llvm::Value *r2 = elimConstExpr(SEL->getFalseValue());

	  if (llvm::isa<llvm::ConstantPointerNull>(r1))
	      *out++ = ruleCode(ruleVar(V) = ruleNull(r1));
	  else if (hasExtraReference(r1))
	      *out++ = ruleCode(ruleVar(V) = &ruleVar(r1));
	  else
	      *out++ = ruleCode(ruleVar(V) = ruleVar(r1));

	  if (llvm::isa<llvm::ConstantPointerNull>(r2))
	      *out++ = ruleCode(ruleVar(V) = ruleNull(r2));
	  else if (hasExtraReference(r2))
	      *out++ = ruleCode(ruleVar(V) = &ruleVar(r2));
	  else
	      *out++ = ruleCode(ruleVar(V) = ruleVar(r2));
      }
    } else if (const llvm::GlobalVariable *GV =
	       llvm::dyn_cast<llvm::GlobalVariable>(V)) {
      const llvm::Value *op = GV->getInitializer();

      *out++ = ruleCode(ruleVar(V) = &ruleVar(op));
    }
  }

}}}

#endif
