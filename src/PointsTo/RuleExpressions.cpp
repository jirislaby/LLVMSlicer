// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "llvm/BasicBlock.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"

#include "RuleExpressions.h"

using namespace llvm;

namespace llvm { namespace ptr { namespace detail {

void buildCallMaps(Module const& M, FunctionsMap& F,
		CallsMap& C) {
    typedef Module::const_iterator FunctionsIter;
    for (FunctionsIter f = M.begin(); f != M.end(); ++f)
    {
	if (!f->isDeclaration())
	    F.insert(std::make_pair(f->getFunctionType(),&*f));
	typedef Function::const_iterator BasicBlocksIter;
	for (BasicBlocksIter b = f->begin(); b != f->end(); ++b)
	{
	    typedef BasicBlock::const_iterator InstructionsIter;
	    for (InstructionsIter i = b->begin(); i != b->end(); ++i)
		if (CallInst const* const c =
			dyn_cast<CallInst>(&*i))
		{
		    if (!isInlineAssembly(c) && !callToMemoryManStuff(c))
			C.insert(std::make_pair(getCalleePrototype(c),c));
		}
		else if (i->getOpcode() == Instruction::Store)
		{
		    Value const* const r = i->getOperand(0);
		    if (hasExtraReference(r) && memoryManStuff(r))
		    {
			Function const* const fn =
				dyn_cast<Function>(r);
			F.insert(std::make_pair(fn->getFunctionType(),fn));
		    }
		}
	}
    }
}

RuleCode argPassRuleCode(Value const* const l,
			 Value const* const r)
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

}}}
